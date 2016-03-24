PROJECT = flying_fox
 
DEPS = jiffy cowboy
dep_jiffy = https://github.com/davisp/jiffy 0.14.7
dep_cowboy = https://github.com/extend/cowboy.git 1.0.3
 
.PHONY: release clean-release
 
release: clean-release all projects
    relx -o rel/$(PROJECT)
 
clean-release: clean-projects
    rm -rf rel/$(PROJECT)

include erlang.mk