REBAR_URL=https://github.com/downloads/basho/rebar/rebar
REBAR=./rebar

all: get-rebar get-deps
	$(REBAR) compile

get-rebar:
	@if [ ! -x rebar ]; then \
		wget $(REBAR_URL) && \
		chmod +x ./rebar; \
	fi

clean:
	$(REBAR) clean

get-deps:
	$(REBAR) get-deps

deb-build:
	dpkg-buildpackage -rfakeroot

deb-clean:
	fakeroot debian/rules clean
