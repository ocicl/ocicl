Name:           <%= @ app-name %>
Version:        0.1.0
Release:        1%{?dist}
Summary:        <%= string-capitalize (@ app-name) %>

License:        <%= (or (@ license) "MIT") %>
URL:            https://github.com/OWNER/<%= @ app-name %>
Source0:        <%= @ app-name %>-%{version}.tar.gz

%global debug_package %{nil}
%global _build_id_links none
%global __strip /bin/true
%global __brp_strip %{nil}
%global __brp_strip_comment_note %{nil}
%global __brp_strip_static_archive %{nil}

BuildRequires:  sbcl
BuildRequires:  gcc
BuildRequires:  make

%description
<%= string-capitalize (@ app-name) %> - a Common Lisp application.

%prep
%autosetup

%build
make

%install
install -D -m 0755 <%= @ app-name %> %{buildroot}%{_bindir}/<%= @ app-name %>
install -D -m 0644 <%= @ app-name %>-sbom.spdx.json %{buildroot}%{_datadir}/sbom/<%= @ app-name %>-%{version}.spdx.json

%files
%license LICENSE
%doc README.md
%{_bindir}/<%= @ app-name %>
%{_datadir}/sbom/<%= @ app-name %>-%{version}.spdx.json

%changelog
