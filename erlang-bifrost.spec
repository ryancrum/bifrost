%global realname bifrost
%global upstream madrat-
%global debug_package %{nil}

%global commit		e0518257d4eada8ee25e730fb15b448e646cab92
%global commit_time 20141113
%global git_tag %(c=%{commit}; echo ${c:0:8})
%global patchnumber 0

%bcond_without check

Name:		erlang-%{realname}
Version:	0.0.0
Release:	%{commit_time}.%{patchnumber}.%{git_tag}%{?dist}
Summary:	Pluggable Erlang FTP Server
Group:		Development/Libraries
License:	MIT
URL:		http://github.com/madrat-/bifrost
#Source0:	%{upstream}-%{realname}-%{version}-%{git_tag}.tar.bz2

BuildRequires:	erlang-rebar
BuildRequires:	erlang-meck >= 0.8.1

Requires:	erlang-compiler%{?_isa}
Requires:	erlang-crypto%{?_isa}
Requires:	erlang-erts%{?_isa} >= R16
Requires:	erlang-inets%{?_isa}
Requires:	erlang-kernel%{?_isa}
Requires:	erlang-ssl%{?_isa}
Requires:	erlang-stdlib%{?_isa} >= R16
Requires:	erlang-syntax_tools%{?_isa}
Requires:	erlang-meck >= 0.8.1
Provides:	%{realname} = %{version}-%{release}


%description
Bifrost is an implementation of the FTP protocol that enables you to create an FTP server without worrying about the protocol details. Many legacy business systems still use FTP heavily for data transmission, and Bifrost can help bridge the gap between them and modern distributed systems. For example, using Bifrost you can pretty easily write an FTP server that serves files from Amazon's S3 and a Postgres SQL server instead of a filesystem.

Bifrost also includes FTP/SSL support, if you supply a certificate.

This version is fork of https://github.com/thorstadt/bifrost

%prep
%setup -T -c -n %{upstream}-%{realname}-%{version}
git clone --depth=1 %{url}  ../%{upstream}-%{realname}-%{version} 
git reset --hard %{commit}

%build
make

%install
install -D -m 644 ebin/%{realname}.app $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/%{realname}.app
install -m 644 ebin/*.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/
install -D -m 644 include/bifrost.hrl $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{realname}-%{version}/include/bifrost.hrl

%check
%if %{with check}
	make test
%endif

%files
%doc LICENSE README.md sample
%dir %{_libdir}/erlang/lib/%{realname}-%{version}
%dir %{_libdir}/erlang/lib/%{realname}-%{version}/ebin
%dir %{_libdir}/erlang/lib/%{realname}-%{version}/include
%{_libdir}/erlang/lib/%{realname}-%{version}/ebin/*
%{_libdir}/erlang/lib/%{realname}-%{version}/include/bifrost.hrl

%changelog
* Thu Nov 13 2014 madrat- 0.0.0
- Initial release
