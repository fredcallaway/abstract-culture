# rsync = rsync -av --exclude .pages --exclude tmp.png
# dest = ~/obsidian/web/cultural-abstractions/figs/v2/
# sync:
# 	${rsync} --delete-after figs/ ${dest}
# 	${rsync} published-figs/ ${dest}
# # 	${rsync} stats/ ~/obsidian/web/cultural-abstractions/stats/
# # 	${rsync} trial_videos/ ~/obsidian/web/cultural-abstractions/trial_videos/