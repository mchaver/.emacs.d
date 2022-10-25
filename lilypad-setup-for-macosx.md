

#+TITLE: Setting up LilyPond

https://old.reddit.com/r/emacs/comments/k0mld2/using_lilypond_in_emacs/

* Setting up lilypond in emacs

This is a terrible experience. A lot of tutorials are outdated, including the one on the official website, even if some of them are up-to-date, they are not for macOS.

Luckily, I found a really detailed guide specified for macOS and emacs.

http://www.danielhensel.de/
​

The following is for backup.

** The original guide

# I modified a bit for better view in org mode.

This is what finally worked for me. Thanks to Orm Finnendahl:

​

Install emacs: http://emacsformacosx.com

​

Install LilyPond:

​

http://www.lilypond.org/index.de.html

​

For absolute beginners: open your terminal. Go to /Applications/Utilities/Terminal.app

​

Lilypond emacs-Setup OS X

​

At first add LilyPond to your PATH:

Open Terminal

# Opening Terminal is not a must, you can stick with the shell in emacs, which is much pleasant to use.

​

#+begin_src sh

sudo touch ~/.bash_profile

#+end_src

​

Open .bash-profile in your favorite editor and fill in:

export PATH="/Applications/LilyPond.app/Contents/Resources/bin:$PATH"

​

# In macOS Catalina, zsh is the default shell, you have no need to shift back to bash. Just create or modify the .zshrc in home directory, paste the same thing.

# This step is very optional, as the official website of LilyPond has another approach, but I prefer this one as it is much simpler.

Save

​

control with echo $PATH

​

Now you can start LilyPond by typing "lilypond" in your terminal.

​

CD to /Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp

​

Type:

​

sudo ln -s /Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp /Applications/Emacs.app/Contents/Resources/site-lisp

​

Now we have to edit lilypond-mode.el. Right click on the LilyPond.app. Show packages: navigate to Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp

# I prefer doing this step in emacs purely. As long as you have permitted emacs to have full access to your disk, you can just C-x C-f /Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp in emacs. But the tricky part for emacs to access the full disk is that, not only you have to permit emacs, but you also have to permit ruby, since emacs probably uses ruby the navigate the disk.

​

Open lilypond-mode.el in emacs

type M-g-g

Go to line 335 and change (defcustom LilyPond-lilypond-command

change to

#+begin_src elisp

(defcustom LilyPond-lilypond-command "/Applications/LilyPond.app/Contents/Resources/bin/lilypond"

"Command used to compile LY files."

:group 'LilyPond

:type 'string)

#+end_src

​

We want to use our Preview.App for PS-Files, so change the above lines, too.

#+begin_src elisp

(defcustom LilyPond-ps-command " /Applications/Preview.app/Contents/MacOS/Preview --watch"

"Command used to display PS files."

​

:group 'LilyPond

:type 'string)

#+end_src

​

The same is with our standard PDF-Viewer, we choose Safari for URL-Handling.

#+begin_src elisp

(defcustom LilyPond-pdf-command "/Applications/Safari.app/Contents/MacOS/Safari“

"Command used to display PDF files."

​

:group 'LilyPond

:type 'string)

#+end_src

​

/* not yet working as expected

​

Change MIDI

#+begin_src elisp

(defcustom LilyPond-midi-command "open"

"Command used to play MIDI files."

#+end_src

​

Change MIDI-Player to Logic Pro X.

#+begin_src elisp

(defcustom LilyPond-all-midi-command "/Applications/Logic\ Pro\ [X.app/Contents/MacOS/Logic\](https://X.app/Contents/MacOS/Logic\) Pro\ X -ia"

"Command used to play MIDI files."

​

:group 'LilyPond

:type 'string)

#+end_src

​

# Since not everyone have purchased Logic Pro X, GarageBand is an free alternative.

​

save with M-x-s

​

on a Mac now type alt-shift-:

​

type

(find-file user-init-file)

# This step is a bit dumb, just go to your home directory and open .emacs, or even C-x C-f ~/.emacs

​

standard lilypond.mode-config-suggestions do not use .ily-files, so I modified them too. Copy and Paste the following to your .emacs

​

in emacs type C-y

​

(setq load-path (append (list (expand-file-name"/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp")) load-path))

(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)

(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))

(add-to-list 'auto-mode-alist '("\\.ily$" . LilyPond-mode))

(add-to-list 'auto-mode-alist '("\\.lytex$" . LilyPond-mode))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

​

(setq locale-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)

(set-keyboard-coding-system 'utf-8)

(set-selection-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

​

restart emacs

​

You are done

​

Open a LilyPond-file in emacs and you'll see!
