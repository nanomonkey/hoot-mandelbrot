(use-modules (guix)
             (guix build-system gnu)
             (guix gexp)
             (guix git)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(define guile-next-next
  (let ((commit "d579848cb5d65440af5afd9c8968628665554c22")
        (revision "1"))
    (package
     (inherit guile-next)
     (version (git-version "3.0.9" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/guile.git")
             (commit commit)))
       (file-name (git-file-name "guile" version))
       (sha256
        (base32 "14a6wpjfsis11l36xjqvq52mbk2m6zdjvw9nba0p916k0pyqr8dm")))))))

(define guile-hoot
  (let ((commit "f52d3ea0bffc1b0aad897a53dca073293ce2d1fb")
        (revision "1"))
    (package
      (name "guile-hoot")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/spritely/guile-hoot.git")
                      (commit commit)))
                (file-name (git-file-name "guile-hoot" version))
                (sha256
                 (base32 "1yyf5i2zbqbmpm094l3xgl4zizq7vjmc58frrcbi7qah7r2wkj3f"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags '("GUILE_AUTO_COMPILE=0")
         #:tests? #f))
      (native-inputs
       (list autoconf automake pkg-config texinfo))
      (inputs
       (list guile-next-next))
      (synopsis "WASM compiler for Guile Scheme")
      (description "Guile-hoot is an ahead-of-time WebAssembly compiler for GNU Guile.")
      (home-page "https://spritely.institute/hoot/")
      (license (list license:asl2.0 license:lgpl3+)))))

(packages->manifest (list guile-next-next guile-hoot gnu-make))
