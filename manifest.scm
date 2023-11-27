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
  (let ((commit "d7cf5bf373392a18e9a4de06f751eae3d66ce1af")
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
        (base32 "05irlc0q7jlx5n5c7xm3cd35azgwyngkdzf46ngs1alnlxibc6kh")))))))

(define guile-hoot
  (let ((commit "06ffffb38fff2bf7671e60fcba4c8d25c581d459")
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
                 (base32 "1cxq24w0bd0w6vc2in67k73pr6zi29riyj1kfgaa9p46khm0fcsl"))))
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
