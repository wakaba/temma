all:

deps: local-submodules

local-submodules:
	-git clone git://github.com/wakaba/wanage local/submodules/wanage
	cd local/submodules/wanage && git pull
	-git clone git://github.com/wakaba/perl-charclass local/submodules/charclass
	cd local/submodules/charclass && git pull
	-git clone git://github.com/wakaba/perl-web-url local/submodules/web-url
	cd local/submodules/web-url && git pull
	-git clone git://github.com/wakaba/perl-web-encodings local/submodules/web-encodings
	cd local/submodules/web-encodings && git pull
