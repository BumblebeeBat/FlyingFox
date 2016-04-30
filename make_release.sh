#This is used to build a release of the software, which is an executable for a particular kind of computer. For example, I use it to build executables on 64-bit linux.

#after running this, the release will be in the /rel directory. you can start it from in the /rel file with the command: 
# `make run`

mkdir rel
cd rel
wget https://raw.githubusercontent.com/ninenines/erlang.mk/master/erlang.mk
cp -r ../deps .
make -f erlang.mk bootstrap bootstrap-rel #LEGACY=1
echo "
PROJECT = flying_fox
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

include erlang.mk
" > Makefile
echo "
{release, {rel_release, \"1\"}, [flying_fox]}.
{extended_start_script, true}.
{sys_config, \"rel/sys.config\"}.
{vm_args, \"rel/vm.args\"}.
" > relx.config
cp -r ../src .
make
mkdir ./_rel/rel_release/data
cp -r ../src ./_rel/rel_release/

