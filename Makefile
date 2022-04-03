all:
	gprbuild
	-git commit -a "-mWorking"
	git push
	ssh   per@piextend-a git -C multicaster pull
	ssh   per@piextend-a gprbuild -P multicaster/multicaster.gpr
	scp   per@piextend-a:multicaster/bin/multicaster multicaster-pi
	scp   multicaster-pi pi@pi-3:multicaster
