rsync = rsync -av --exclude .pages --exclude tmp.png

papersync:
	${rsync} figs/paper/ ~/papers/cultural-abstractions/figs/ --exclude *.png

websync:
	dest=~/obsidian/web/cultural-abstractions/figs/v2/
	${rsync} --delete-after figs/ ${dest}
	${rsync} published-figs/ ${dest}
# 	${rsync} stats/ ~/obsidian/web/cultural-abstractions/stats/
# 	${rsync} trial_videos/ ~/obsidian/web/cultural-abstractions/trial_videos/