rsync = rsync --delete-after -av --exclude .pages --exclude tmp.png
sync:
	${rsync} figs/ ~/obsidian/web/cultural-abstractions/figs/v2/
# 	${rsync} stats/ ~/obsidian/web/cultural-abstractions/stats/
# 	${rsync} trial_videos/ ~/obsidian/web/cultural-abstractions/trial_videos/