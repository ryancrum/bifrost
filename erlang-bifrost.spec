%global realname bifrost
%global upstream madrat-
%global debug_package %{nil}

%global git_url https://github.com/%{upstream}/%{realname}

%if 0%{!?git_tag:1}
%global git_tag HEAD
%endif

%global git_log %(GITDIR=`mktemp -d` && git clone -q %{git_url} $GITDIR && pushd $GITDIR >/dev/null && git reset -q --hard %{git_tag} && LOG=$(git log -n 1 --format='%h %H %ct') && popd >/dev/null && rm -Rf $GITDIR >/dev/null && echo $LOG)

%if "%{git_log}" == ""
	Can not get log info for %{git_tag}
%endif

%global git_tag 		%(echo "%{git_log}" | cut -d ' ' -f1)
%global git_commit  	%(echo "%{git_log}" | cut -d ' ' -f2)
%global git_commit_time %(echo "%{git_log}" | cut -d ' ' -f3 | date --utc -d - +'%Y%m%d')

%{echo:Building commit %{git_commit}...
}

%global patchnumber 0

%bcond_without check

Name:		erlang-%{realname}
Version:	0.0.0
Release:	%{git_commit_time}.%{patchnumber}.%{git_tag}%{?dist}
Summary:	Pluggable Erlang FTP Server
Group:		Development/Libraries
License:	MIT
URL:		%{git_url}

BuildRequires:	erlang-rebar
%{!?_without_check:BuildRequires:  erlang-meck >= 0.8.1}

Requires:	erlang-compiler%{?_isa}
Requires:	erlang-crypto%{?_isa}
Requires:	erlang-erts%{?_isa} >= R16
Requires:	erlang-inets%{?_isa}
Requires:	erlang-kernel%{?_isa}
Requires:	erlang-ssl%{?_isa}
Requires:	erlang-stdlib%{?_isa} >= R16
Requires:	erlang-syntax_tools%{?_isa}
Provides:	%{realname} = %{version}-%{release}


%description
Bifrost is an implementation of the FTP protocol that enables you to create an FTP server without worrying about the protocol details. Many legacy business systems still use FTP heavily for data transmission, and Bifrost can help bridge the gap between them and modern distributed systems. For example, using Bifrost you can pretty easily write an FTP server that serves files from Amazon's S3 and a Postgres SQL server instead of a filesystem.

Bifrost also includes FTP/SSL support, if you supply a certificate.

This version is fork of https://github.com/thorstadt/bifrost

%prep
%setup -n %{upstream}-%{realname}-%{version} -T -c
git clone -q %{git_url} `pwd`
git reset -q --hard %{git_tag}
%{?_without_check:sed -rne '/meck/!p' -i.meck rebar.config}

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
