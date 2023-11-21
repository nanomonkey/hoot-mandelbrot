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
  (let ((commit "49aa0940bcd1f77819326e73aaee44f5f359d830")
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
        (base32 "164i4q0vcb3i790fx6fa9ajjlnx253cabgan2m92kigbpid988hi")))))))

(define guile-hoot
  (let ((commit "bfe760073151f6e4bd2161b32d6e6f28706df9eb")
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
                 (base32 "05fik20y9v7dz0xamlpas3xp3fwx7qn5n2lrf2qgm6byk2crwxnv"))))
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
