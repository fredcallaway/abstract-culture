rsync = rsync --delete-after -av --exclude .pages --exclude tmp
sync:
	${rsync} figs/ ~/obsidian/web/cultural-abstractions/figs/
# 	${rsync} stats/ ~/obsidian/web/cultural-abstractions/stats/
# 	${rsync} trial_videos/ ~/obsidian/web/cultural-abstractions/trial_videos/