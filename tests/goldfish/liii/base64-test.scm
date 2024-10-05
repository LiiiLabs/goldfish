(import (liii check)
        (liii base64))

(check (string-base64-encode "") => "")
(check (string-base64-encode "a") => "YQ==")
(check (string-base64-encode "z") => "eg==")
(check (string-base64-encode "f") => "Zg==")
(check (string-base64-encode "fo") => "Zm8=")
(check (string-base64-encode "foo") => "Zm9v")
(check (string-base64-encode "foob") => "Zm9vYg==")
(check (string-base64-encode "fooba") => "Zm9vYmE=")
(check (string-base64-encode "foobar") => "Zm9vYmFy")

(check-report)

