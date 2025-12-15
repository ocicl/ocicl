Name:           ocicl
Version:        2.8.0
Release:        1%{?dist}
Summary:        A modern ASDF system distribution and management tool for Common Lisp

License:        MIT
URL:            https://github.com/ocicl/ocicl
Source0:        ocicl-%{version}.tar.gz

# Disable debug packages and stripping since this is a Lisp binary with dumped image
%global debug_package %{nil}
%global _build_id_links none
%global __strip /bin/true
%global __brp_strip %{nil}
%global __brp_strip_comment_note %{nil}
%global __brp_strip_static_archive %{nil}

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
# Collect licenses from vendored dependencies
./ocicl collect-licenses >VENDORED-LICENSES.txt

# Install the binary
install -D -m 0755 ocicl %{buildroot}%{_bindir}/ocicl

# Install licenses
install -D -m 0644 VENDORED-LICENSES.txt %{buildroot}%{_datadir}/licenses/%{name}/VENDORED-LICENSES.txt

%files
%license LICENSE
%{_datadir}/licenses/%{name}/VENDORED-LICENSES.txt
%doc README.md
%{_bindir}/ocicl

%post
# Run setup for system-wide installation (optional)
echo "Run 'ocicl setup' to complete installation"

%changelog
* Wed Oct 15 2025 Anthony Green <green@moxielogic.com> - 2.8.0-1
- Add native Linux package distribution (RPM and DEB)
- Add vendored license collection for all dependencies
- Improve CI/CD with automated package building

* Wed Oct 15 2025 Anthony Green <green@moxielogic.com> - 2.7.9-1
- Fix Windows TLS verification default (Fixes #147)

* Tue Oct 14 2025 Anthony Green <green@moxielogic.com> - 2.7.8-1
- Add five new linting rules
- Fix ASDF system name to use string instead of symbol
- Credit Eitaro Fukamachi for linting rule inspiration

* Mon Oct 13 2025 Anthony Green <green@moxielogic.com> - 2.7.7-1
- Fix linter path resolution when running from saved core image
- Fix linter false positives for character literals and lambda lists
