# pure-tls 1.12.0

**Release date:** 2026-07-06

Security-hardening release. Two fail-closed guards tighten certificate
handling: hostname verification now rejects unsafe DNS names outright,
and naming an unusable explicit trust source is now an error rather
than a silent trust-nothing store. Both were contributed by @fade.

## Security Fixes

- **Reject embedded-NUL and non-LDH bytes in hostname verification.**
  A name-safety guard is now applied to the requested identity and to
  each candidate SAN `dNSName`. A name containing an embedded NUL, or
  any byte outside the LDH set (letters, digits, hyphen, dot), is
  rejected rather than reaching a silent unequal compare — closing the
  classic `www.bank.com\0.evil.com` truncation-confusion class. A
  single leading `*.` wildcard label is still permitted, so legitimate
  wildcard SAN patterns continue to validate. The check runs after
  IDNA normalization, so Unicode (U-label) hostnames convert to their
  `xn--` A-label form and verify as before, while an embedded NUL or
  other non-LDH byte survives normalization and is still rejected
  (#14).

- **Fail closed on an unusable explicit CA source.** Naming an explicit
  `:ca-file` or `:ca-directory` that is unreadable, empty, or malformed
  previously produced a silent trust-nothing store: verification then
  failed every certificate with no indication that the configured trust
  source was the cause. pure-tls now signals `tls-certificate-error` at
  context creation, so a misconfigured explicit source is a clear error
  rather than a confusing downstream verification failure. The
  system-CA auto-load path is unchanged (still warn-and-continue); only
  an explicitly named source is treated as fail-closed (#13).

## Acknowledgments

Both hardening fixes were contributed by @fade.
