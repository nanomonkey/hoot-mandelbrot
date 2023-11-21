// -*- js2-basic-offset: 4 -*-
class Char {
    constructor(codepoint) {
        this.codepoint = codepoint;
    }
    toString() {
        let ch = String.fromCodePoint(this.codepoint);
        if (ch.match(/[a-zA-Z0-9$[\]().]/)) return `#\\${ch}`;
        return `#\\x${this.codepoint.toString(16)}`;
    }
}
class Eof { toString() { return "#<eof>"; } }
class Nil { toString() { return "#nil"; } }
class Null { toString() { return "()"; } }
class Unspecified { toString() { return "#<unspecified>"; } }

class Complex {
    constructor(real, imag) {
        this.real = real;
        this.imag = imag;
    }
    toString() {
        return `${flonum_to_string(this.real)}+${flonum_to_string(this.imag)}i`;
    }
}
class Fraction {
    constructor(num, denom) {
        this.num = num;
        this.denom = denom;
    }
    toString() {
        return `${this.num}/${this.denom}`;
    }
}

class HeapObject {
    constructor(reflector, obj) {
        this.reflector = reflector;
        this.obj = obj;
    }
    repr() { return this.toString(); } // Default implementation.
}

class Pair extends HeapObject {
    toString() { return "#<pair>"; }
    repr() {
        let car_repr = repr(this.reflector.car(this));
        let cdr_repr = repr(this.reflector.cdr(this));
        if (cdr_repr == '()')
            return `(${car_repr})`;
        if (cdr_repr.charAt(0) == '(')
            return `(${car_repr} ${cdr_repr.substring(1)}`;
        return `(${car_repr} . ${cdr_repr})`;
    }
}
class MutablePair extends Pair { toString() { return "#<mutable-pair>"; } }

class Vector extends HeapObject {
    toString() { return "#<vector>"; }
    repr() {
        let len = this.reflector.vector_length(this);
        let out = '#(';
        for (let i = 0; i < len; i++) {
            if (i) out += ' ';
            out += repr(this.reflector.vector_ref(this, i));
        }
        out += ')';
        return out;
    }
}
class MutableVector extends Vector {
    toString() { return "#<mutable-vector>"; }
}

class Bytevector extends HeapObject {
    toString() { return "#<bytevector>"; }
    repr() {
        let len = this.reflector.bytevector_length(this);
        let out = '#vu8(';
        for (let i = 0; i < len; i++) {
            if (i) out += ' ';
            out += this.reflector.bytevector_ref(this, i);
        }
        out += ')';
        return out;
    }
}
class MutableBytevector extends Bytevector {
    toString() { return "#<mutable-bytevector>"; }
}

class Bitvector extends HeapObject {
    toString() { return "#<bitvector>"; }
    repr() {
        let len = this.reflector.bitvector_length(this);
        let out = '#*';
        for (let i = 0; i < len; i++) {
            out += this.reflector.bitvector_ref(this, i) ? '1' : '0';
        }
        return out;
    }
}
class MutableBitvector extends Bitvector {
    toString() { return "#<mutable-bitvector>"; }
}

class MutableString extends HeapObject {
    toString() { return "#<mutable-string>"; }
    repr() { return this.reflector.string_value(this); }
}

class Procedure extends HeapObject {
    toString() { return "#<procedure>"; }
    call(...arg) {
        return this.reflector.call(this, ...arg);
    }
}

class Sym extends HeapObject {
    toString() { return "#<symbol>"; }
    repr() { return this.reflector.symbol_name(this); }
}

class Keyword extends HeapObject {
    toString() { return "#<keyword>"; }
    repr() { return `#:${this.reflector.keyword_name(this)}`; }
}

class Variable extends HeapObject { toString() { return "#<variable>"; } }
class AtomicBox extends HeapObject { toString() { return "#<atomic-box>"; } }
class HashTable extends HeapObject { toString() { return "#<hash-table>"; } }
class WeakTable extends HeapObject { toString() { return "#<weak-table>"; } }
class Fluid extends HeapObject { toString() { return "#<fluid>"; } }
class DynamicState extends HeapObject { toString() { return "#<dynamic-state>"; } }
class Syntax extends HeapObject { toString() { return "#<syntax>"; } }
class Port extends HeapObject { toString() { return "#<port>"; } }
class Struct extends HeapObject { toString() { return "#<struct>"; } }

function instantiate_streaming(path, imports) {
    if (typeof fetch !== 'undefined')
        return WebAssembly.instantiateStreaming(fetch(path), imports);
    let bytes;
    if (typeof read !== 'undefined') {
        bytes = read(path, 'binary');
    } else if (typeof readFile !== 'undefined') {
        bytes = readFile(path);
    } else {
        let fs = require('fs');
        bytes = fs.readFileSync(path);
    }
    return WebAssembly.instantiate(bytes, imports);
}

class Scheme {
    #instance;
    #abi;
    constructor(instance, abi) {
        this.#instance = instance;
        this.#abi = abi;
    }

    static async reflect(abi) {
        let { module, instance } =
            await instantiate_streaming('js-runtime/reflect.wasm', {
              abi,
              rt: {
                  wtf8_to_string(wtf8) { return wtf8_to_string(wtf8); },
                  string_to_wtf8(str) { return string_to_wtf8(str); },
              }
            });
        return new Scheme(instance, abi);
    }

    #init_module(mod) {
        mod.set_debug_handler({
            debug_str(x) { console.log(`debug: ${x}`); },
            debug_str_i32(x, y) { console.log(`debug: ${x}: ${y}`); },
            debug_str_scm: (x, y) => {
                console.log(`debug: ${x}: ${repr(this.#to_js(y))}`);
            },
        });
        mod.set_ffi_handler({
            procedure_to_extern: (obj) => {
                const proc = this.#to_js(obj);
                return (...args) => {
                    return proc.call(...args);
                };
            }
        });
        let proc = new Procedure(this, mod.get_export('$load').value);
        return proc.call();
    }
    static async load_main(path, abi, user_imports = {}) {
        let mod = await SchemeModule.fetch_and_instantiate(path, abi, user_imports);
        let reflect = await mod.reflect();
        return reflect.#init_module(mod);
    }
    async load_extension(path, user_imports = {}) {
        let mod = await SchemeModule.fetch_and_instantiate(path, this.#abi, user_imports);
        return this.#init_module(mod);
    }

    #to_scm(js) {
        let api = this.#instance.exports;
        if (typeof(js) == 'number') {
            return api.scm_from_f64(js);
        } else if (typeof(js) == 'bigint') {
            if (BigInt(api.scm_most_negative_fixnum()) <= js
                && js <= BigInt(api.scm_most_positive_fixnum()))
                return api.scm_from_fixnum(Number(js));
            return api.scm_from_bignum(js);
        } else if (typeof(js) == 'boolean') {
            return js ? api.scm_true() : api.scm_false();
        } else if (typeof(js) == 'string') {
            return api.scm_from_string(js);
        } else if (typeof(js) == 'object') {
            if (js instanceof Eof) return api.scm_eof();
            if (js instanceof Nil) return api.scm_nil();
            if (js instanceof Null) return api.scm_null();
            if (js instanceof Unspecified) return api.scm_unspecified();
            if (js instanceof Char) return api.scm_from_char(js.codepoint);
            if (js instanceof HeapObject) return js.obj;
            if (js instanceof Fraction)
                return api.scm_from_fraction(this.#to_scm(js.num),
                                             this.#to_scm(js.denom));
            if (js instanceof Complex)
                return api.scm_from_complex(js.real, js.imag);
            return api.scm_from_extern(js);
        } else {
            throw new Error(`unexpected; ${typeof(js)}`);
        }
    }

    #to_js(scm) {
        let api = this.#instance.exports;
        let descr = api.describe(scm);
        let handlers = {
            fixnum: () => BigInt(api.fixnum_value(scm)),
            char: () => new Char(api.char_value(scm)),
            true: () => true,
            false: () => false,
            eof: () => new Eof,
            nil: () => new Nil,
            null: () => new Null,
            unspecified: () => new Unspecified,
            flonum: () => api.flonum_value(scm),
            bignum: () => api.bignum_value(scm),
            complex: () => new Complex(api.complex_real(scm),
                                       api.complex_imag(scm)),
            fraction: () => new Fraction(this.#to_js(api.fraction_num(scm)),
                                         this.#to_js(api.fraction_denom(scm))),
            pair: () => new Pair(this, scm),
            'mutable-pair': () => new MutablePair(this, scm),
            vector: () => new Vector(this, scm),
            'mutable-vector': () => new MutableVector(this, scm),
            bytevector: () => new Bytevector(this, scm),
            'mutable-bytevector': () => new MutableBytevector(this, scm),
            bitvector: () => new Bitvector(this, scm),
            'mutable-bitvector': () => new MutableBitvector(this, scm),
            string: () => api.string_value(scm),
            'mutable-string': () => new MutableString(this, scm),
            procedure: () => new Procedure(this, scm),
            symbol: () => new Sym(this, scm),
            keyword: () => new Keyword(this, scm),
            variable: () => new Variable(this, scm),
            'atomic-box': () => new AtomicBox(this, scm),
            'hash-table': () => new HashTable(this, scm),
            'weak-table': () => new WeakTable(this, scm),
            fluid: () => new Fluid(this, scm),
            'dynamic-state': () => new DynamicState(this, scm),
            syntax: () => new Syntax(this, scm),
            port: () => new Port(this, scm),
            struct: () => new Struct(this, scm),
            'extern-ref': () => api.extern_value(scm)
        };
        let handler = handlers[descr];
        return handler ? handler() : scm;
    }

    call(func, ...args) {
        let api = this.#instance.exports;
        let argv = api.make_vector(args.length + 1, api.scm_false());
        func = this.#to_scm(func);
        api.vector_set(argv, 0, func);
        for (let [idx, arg] of args.entries())
            api.vector_set(argv, idx + 1, this.#to_scm(arg));
        argv = api.call(func, argv);
        let results = [];
        for (let idx = 0; idx < api.vector_length(argv); idx++)
            results.push(this.#to_js(api.vector_ref(argv, idx)))
        return results;
    }

    car(x) { return this.#to_js(this.#instance.exports.car(x.obj)); }
    cdr(x) { return this.#to_js(this.#instance.exports.cdr(x.obj)); }

    vector_length(x) { return this.#instance.exports.vector_length(x.obj); }
    vector_ref(x, i) {
        return this.#to_js(this.#instance.exports.vector_ref(x.obj, i));
    }

    bytevector_length(x) {
        return this.#instance.exports.bytevector_length(x.obj);
    }
    bytevector_ref(x, i) {
        return this.#instance.exports.bytevector_ref(x.obj, i);
    }

    bitvector_length(x) {
        return this.#instance.exports.bitvector_length(x.obj);
    }
    bitvector_ref(x, i) {
        return this.#instance.exports.bitvector_ref(x.obj, i) == 1;
    }

    string_value(x) { return this.#instance.exports.string_value(x.obj); }
    symbol_name(x) { return this.#instance.exports.symbol_name(x.obj); }
    keyword_name(x) { return this.#instance.exports.keyword_name(x.obj); }
}

class SchemeTrapError extends Error {
    constructor(tag, data) { super(); this.tag = tag; this.data = data; }
    // FIXME: data is raw Scheme object; would need to be reflected to
    // have a toString.
    toString() { return `SchemeTrap(${this.tag}, <data>)`; }
}

function flonum_to_string(f64) {
    if (Object.is(f64, -0)) {
        return '-0.0';
    } else if (Number.isFinite(f64)) {
        let repr = f64 + '';
        return /^-?[0-9]+$/.test(repr) ? repr + '.0' : repr;
    } else if (Number.isNaN(f64)) {
        return '+nan.0';
    } else {
        return f64 < 0 ? '-inf.0' : '+inf.0';
    }
}

let wtf8_helper;

function wtf8_to_string(wtf8) {
    let { as_iter, iter_next } = wtf8_helper.exports;
    let codepoints = [];
    let iter = as_iter(wtf8);
    for (let cp = iter_next(iter); cp != -1; cp = iter_next(iter))
        codepoints.push(cp);
    return String.fromCodePoint(...codepoints);
}

function string_to_wtf8(str) {
    let { string_builder, builder_push_codepoint, finish_builder } =
        wtf8_helper.exports;
    let builder = string_builder()
    for (let cp of str)
        builder_push_codepoint(builder, cp.codePointAt(0));
    return finish_builder(builder);
}

async function load_wtf8_helper_module() {
    if (wtf8_helper) return;
    let { module, instance } = await instantiate_streaming("js-runtime/wtf8.wasm");
    wtf8_helper = instance;
}

class SchemeModule {
    #instance;
    #io_handler;
    #debug_handler;
    #ffi_handler;
    static #rt = {
        bignum_from_string(str) { return BigInt(str); },
        bignum_from_i32(n) { return BigInt(n); },
        bignum_from_i64(n) { return n; },
        bignum_from_u64(n) { return n < 0n ? 0xffff_ffff_ffff_ffffn + (n + 1n) : n; },
        bignum_is_i64(n) {
            return -0x8000_0000_0000_0000n <= n && n <= 0x7FFF_FFFF_FFFF_FFFFn;
        },
        bignum_is_u64(n) {
            return 0n <= n && n <= 0xFFFF_FFFF_FFFF_FFFFn;
        },
        // This truncates; see https://tc39.es/ecma262/#sec-tobigint64.
        bignum_get_i64(n) { return n; },

        bignum_add(a, b) { return BigInt(a) + BigInt(b) },
        bignum_sub(a, b) { return BigInt(a) - BigInt(b) },
        bignum_mul(a, b) { return BigInt(a) * BigInt(b) },
        bignum_lsh(a, b) { return BigInt(a) << BigInt(b) },
        bignum_rsh(a, b) { return BigInt(a) >> BigInt(b) },
        bignum_quo(a, b) { return BigInt(a) / BigInt(b) },
        bignum_rem(a, b) { return BigInt(a) % BigInt(b) },
        bignum_mod(a, b) {
            let r = BigInt(a) % BigInt(b);
            if ((b > 0n && r < 0n) || (b < 0n && r > 0n)) {
                return b + r;
            } else {
                return r;
            }
        },
        bignum_gcd(a, b) {
            a = BigInt(a);
            b = BigInt(b);
            if (a < 0n) { a = -a; }
            if (b < 0n) { b = -b; }
            if (a == 0n) { return b; }
            if (b == 0n) { return a; }

            let r;
            while (b != 0n) {
                r = a % b;
                a = b;
                b = r;
            }
            return a;
        },

        bignum_logand(a, b) { return BigInt(a) & BigInt(b); },
        bignum_logior(a, b) { return BigInt(a) | BigInt(b); },
        bignum_logxor(a, b) { return BigInt(a) ^ BigInt(b); },
        bignum_logsub(a, b) { return BigInt(a) & (~ BigInt(b)); },

        bignum_lt(a, b) { return a < b; },
        bignum_le(a, b) { return a <= b; },
        bignum_eq(a, b) { return a == b; },

        bignum_to_f64(n) { return Number(n); },

        f64_is_nan(n) { return Number.isNaN(n); },
        f64_is_infinite(n) { return !Number.isFinite(n); },

        flonum_to_string,

        string_upcase: Function.call.bind(String.prototype.toUpperCase),
        string_downcase: Function.call.bind(String.prototype.toLowerCase),

        make_weak_map() { return new WeakMap; },
        weak_map_get(map, k) { return map.get(k) || null; },
        weak_map_set(map, k, v) { return map.set(k, v); },
        weak_map_delete(map, k) { return map.delete(k); },

        fsqrt: Math.sqrt,
        fsin: Math.sin,
        fcos: Math.cos,
        ftan: Math.tan,
        fasin: Math.asin,
        facos: Math.acos,
        fatan: Math.atan,
        fatan2: Math.atan2,
        flog: Math.log,
        fexp: Math.exp,

        jiffies_per_second() { return 1000; },
        current_jiffy() { return BigInt(Math.floor(performance.now())); },
        current_second() { return Date.now() / 1000; },

        // Wrap in functions to allow for lazy loading of the wtf8
        // module.
        wtf8_to_string(wtf8) { return wtf8_to_string(wtf8); },
        string_to_wtf8(str) { return string_to_wtf8(str); },

        die(tag, data) { throw new SchemeTrapError(tag, data); }
    };

    constructor(instance) {
        this.#instance = instance;
        let read_stdin = () => '';
        if (typeof printErr === 'function') {
            // On the console, try to use 'write' (v8) or 'putstr' (sm),
            // as these don't add an extraneous newline.  Unfortunately
            // JSC doesn't have a printer that doesn't add a newline.
            let write_no_newline =
                typeof write === 'function' ? write
                : typeof putstr === 'function' ? putstr : print;
            this.#io_handler = {
                write_stdout: write_no_newline,
                write_stderr: printErr,
                read_stdin
            };
        } else {
            this.#io_handler = {
                write_stdout: console.log,
                write_stderr: console.error,
                read_stdin
            }
        }
        this.#debug_handler = {
            debug_str(x) { console.log(`debug: ${x}`); },
            debug_str_i32(x, y) { console.log(`debug: ${x}: ${y}`); },
            debug_str_scm(x, y) { console.log(`debug: ${x}: #<scm>`); },
        }
    }
    static async fetch_and_instantiate(path, imported_abi, user_imports = {}) {
        await load_wtf8_helper_module();
        let io = {
            write_stdout(str) { mod.#io_handler.write_stdout(str); },
            write_stderr(str) { mod.#io_handler.write_stderr(str); },
            read_stdin() { return mod.#io_handler.read_stdin(); },
        }
        let debug = {
            debug_str(x) { mod.#debug_handler.debug_str(x); },
            debug_str_i32(x, y) { mod.#debug_handler.debug_str_i32(x, y); },
            debug_str_scm(x, y) { mod.#debug_handler.debug_str_scm(x, y); },
        }
        let ffi = {
            procedure_to_extern(proc) {
                return mod.#ffi_handler.procedure_to_extern(proc);
            }
        };
        let imports = {
          rt: SchemeModule.#rt,
          abi: imported_abi,
          debug, io, ffi, ...user_imports
        };
        let { module, instance } = await instantiate_streaming(path, imports);
        let mod = new SchemeModule(instance);
        return mod;
    }
    set_io_handler(h) { this.#io_handler = h; }
    set_debug_handler(h) { this.#debug_handler = h; }
    set_ffi_handler(h) { this.#ffi_handler = h; }
    all_exports() { return this.#instance.exports; }
    exported_abi() {
        let abi = {}
        for (let [k, v] of Object.entries(this.all_exports())) {
            if (k.startsWith("$"))
                abi[k] = v;
        }
        return abi;
    }
    exports() {
        let ret = {}
        for (let [k, v] of Object.entries(this.all_exports())) {
            if (!k.startsWith("$"))
                ret[k] = v;
        }
        return ret;
    }
    get_export(name) {
        if (name in this.all_exports())
            return this.all_exports()[name];
        throw new Error(`unknown export: ${name}`)
    }
    async reflect() {
        return await Scheme.reflect(this.exported_abi());
    }
}

function repr(obj) {
    if (obj instanceof HeapObject)
        return obj.repr();
    if (typeof obj === 'boolean')
        return obj ? '#t' : '#f';
    if (typeof obj === 'number')
        return flonum_to_string(obj);
    if (typeof obj === 'string')
        // FIXME: Improve to match Scheme.
        return '"' + obj.replace(/(["\\])/g, '\\$1').replace(/\n/g, '\\n') + '"';
    return obj + '';
}
