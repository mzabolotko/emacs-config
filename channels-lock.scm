(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "f44a49d3092d509c5e09cc27ada40a8c4d99c02d")
        (introduction
          (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
         (name 'mz-channel)
	 (url "https://github.com/mzabolotko/guix-channel")
	 (branch "master")
	 (commit "f7bcc0a87688575aa01984b88f0c19c261435809")
	 (introduction
 	 (make-channel-introduction
	     "e7d3bac10e8696117fa4c20d2f7e2ffd3f8c4897"
	     (openpgp-fingerprint
	       "1B7E 1A8F 551F BBB8 7E83  A040 C9F6 5395 68C2 EB28"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "40b9f330662cdccfa3d279aecc2f447d39f858d4")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
