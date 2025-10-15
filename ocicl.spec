Name:           ocicl
Version:        2.7.9
Release:        1%{?dist}
Summary:        A modern ASDF system distribution and management tool for Common Lisp

License:        MIT
URL:            https://github.com/ocicl/ocicl
Source0:        https://github.com/ocicl/ocicl/archive/v%{version}.tar.gz

BuildRequires:  sbcl
BuildRequires:  libfixposix-devel
BuildRequires:  gcc
BuildRequires:  make

%description
ocicl is a modern tool for Common Lisp development that provides:
* Package management: A modern alternative to quicklisp for ASDF system
  distribution and management
* Code linting: An integrated linter that checks your code for style issues,
  common errors, and best practices
* Project scaffolding: Template-based project creation to quickly start new
  applications

All software is packaged as OCI-compliant artifacts and distributed from
mirrored OCI-compliant registries. All software packages are securely
distributed over TLS connections and sigstore is used to ensure the integrity
and authenticity of all software packages.

%prep
%autosetup

%build
# Build ocicl using the provided setup script
sbcl --load setup.lisp --eval "(sb-ext:quit)" || true

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_bindir}

# Install the binary
install -m 0755 ocicl $RPM_BUILD_ROOT%{_bindir}/ocicl

%files
%license LICENSE
%doc README.md
%{_bindir}/ocicl

%post
# Run setup for system-wide installation (optional)
echo "Run 'ocicl setup' to complete installation"

%changelog
* Wed Oct 15 2025 Anthony Green <green@moxielogic.com> - 2.7.9-1
- Fix Windows TLS verification default (Fixes #147)

* Tue Oct 14 2025 Anthony Green <green@moxielogic.com> - 2.7.8-1
- Add five new linting rules
- Fix ASDF system name to use string instead of symbol
- Credit Eitaro Fukamachi for linting rule inspiration

* Mon Oct 13 2025 Anthony Green <green@moxielogic.com> - 2.7.7-1
- Fix linter path resolution when running from saved core image
- Fix linter false positives for character literals and lambda lists
