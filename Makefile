install:
	git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
	~/.emacs.d/bin/doom install

sync:
	~/.emacs.d/bin/doom sync
