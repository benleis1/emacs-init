# Introduction

When I first started using LSP, jdtls (the java language server) and emacs, I installed lsp-mode because it was recommended online in the various places I first looked (youtube, reddit etc.). After the normal configuration issues were ironed out  that worked fine for my initial test scenarios. But recently, I've decided that lsp-mode makes some design choices that around integration with UI, treemacs and file layouts etc. that I'm not sure I like and it would be fun to experiment with the built-in eglot and see how it compares. A recent regression in the  diagnostics build warnings/errors side window's ability to move to the line via a right click  for each issue while fixable was the final motivation for me to give eglot a try. This was also an opportunity to reexamine some configuration I have never fully cleaned up in the first place.

# Initial Chaos
To begin with after making sure I was at a clean state with my config files in git, I deleted all the lsp-mode related lines in my init.el and then went to a sample java file and invoked eglot. Since its built-in I didn't need to even add a `use-package` declaration.  And then predictably **chaos** ensued.  More accurately at first nothing happened.  After examining the messages buffer I realized my issue.   First, I thought the jdtls artifacts i had previously used with lsp-mode would just work when I transitioned but they were not found initially by eglot. Also by this time I had long since forgotten where lsp-mode placed them.  I prefer my binaries to be by convention  in the same general location, so rather than relying on an emacs package to install jdtls to some one off directory under emacs.d, I directly installed it via homebrew. That's a more sustainable management pattern for me for the long term. I can use `which` to find it, `homebrew upgrade` to keep it up to date like all other programs  and it just runs off the command line if I invoke  `jdtls. As a side benefit, no path configuration is needed within emacs to start it.
	
With that in place, I started over, invoked eglot and my emacs just silently froze for about a minute before sputtering out an error message	about timing out and  shutting down the server.   I have to deal with fairly large git repos at work and it takes time to load and process them.  So the next step was bumping up the initial connection timeout along with limiting the number of files to watch to reasonable cap.

```
(setq eglot-connect-timeout 180)
(setq eglot-max-file-watches 5000)
```

What also became apparent from the bit of output I found in the logs (see below) was more files were being processed than I expected. This is due to the file structure at work which heavily uses gradle subprojects.   But by default `project.el` sets the root of a project to the repo's git root and that's also the root of the workspace that eglot uses for compilation.  In other words, I was pulling in the universe each time I opened a file.  The fix here was to tell project that the boundary was at the first build.gradle file it found as well as the git root  so only the  subproject I was using was processed by jdtls. I was curious if this was going to work since the subprojects do refer to common  gradle files up in the parent but thankfully there were no gotchas this time.

```
;; Set project boundary at the first build.gradle found as well
(setopt project-vc-extra-root-markers '("build.gradle"))
```


# Formatting JSON-RPC

Eglot was connecting fine if I waited long enough now. But it still was a disconcerting experience with almost no UI feedback as to what was going on. Side note: I found it hard to find documentation giving you a picture of the expected experience to compare which is part of my motivation for this writeup.  What I found that was extremely useful  during my experimentation is the *EGLOT ...* diagnostic buffer which contains a raw dump of the messages between emacs and the LSP server. However, there are a lot of messages going back and its not that easy to parse at a glance. It turns out there are 3 formats for this buffer which can be configured via `eglot-events-buffer-config `:

1. full - dumps the json as is and is the default
2. lisp - formats the json as a lisp structure
3. short - gives a short one line summary

In theory the short format sounded helpful but it unfortunately hides the message fields that contain most of the useful information.  So I instead wrote a custom filter to focus on presenting a timestamp and the description field and to filter out a few of the begin/end message pairs that aren't normally useful.

```
[jsonrpc] e[23:22:53.541] <-- initialize[1] 
[jsonrpc] e[23:22:53.541] --> initialized #s(hash-table)
[jsonrpc] e[23:22:53.569] <-- window/workDoneProgress/create[1] (:token <elided>)
[jsonrpc] e[23:22:53.573] <-- window/workDoneProgress/create[2] (:token <elided>)
[jsonrpc] e[23:22:53.573] <-- $/progress report: Building - 0% 
[jsonrpc] e[23:22:53.594] <-- language/status (:type Starting :message Init...)
[jsonrpc] e[23:22:53.594] <-- language/status (:type Starting :message 0% Starting Java Language Server)
[jsonrpc] e[23:22:53.594] <-- window/workDoneProgress/create[3] (:token <elided>)
[jsonrpc] e[23:22:53.595] <-- $/progress report: Initialize Workspace - 0% 
[jsonrpc] e[23:22:53.595] <-- window/workDoneProgress/create[4] (:token <elided>)
[jsonrpc] e[23:22:54.271] <-- language/status (:type Starting :message 70% Starting Java Language Server)
[jsonrpc] e[23:22:54.271] <-- $/progress report: Importing Gradle project(s) - 70% 
[jsonrpc] e[23:22:55.770] <-- language/eventNotification (:eventType 200 :data [file:/Users/benjamin.leis/dev/jdk17/glide/glide/ file:/Users/benjamin.leis/dev/jdk17/glide/glide/ui.html/styles/polaris/])
[jsonrpc] e[23:23:03.790] <-- language/status (:type Starting :message 70% Starting Java Language Server - Configure build)
[jsonrpc] e[23:23:03.790] <-- $/progress report: Importing root project - 70% Configure build
[jsonrpc] e[23:23:04.289] <-- language/status (:type Starting :message 70% Starting Java Language Server - Configure project :buildSrcCommon:buildSrc)cccccbvnnbrdecrgirrjlnfdtvfjdvchldrbdenrfjjh

[jsonrpc] e[23:23:04.289] <-- $/progress report: Importing root project - 70% Configure project :buildSrcCommon:buildSrc
```

This is a bit heavyweight and I'm deciding if I will continue to use it but for the initial debugging of the process it sped things up quite a bit. So now I could park on this buffer during startup and watch the process during the UI freezes and also see some indication of when import had finished.  I also discovered by this point, it's only  an issue during initial import and reconnects are asynchronous.   This seems to be an artifact of the LSP protocol, fundamental eglot design and emacs relatively weak support for threading and I don't think I can tune it further  (but  I'm curious if anyone has found something for a truly asynchronous connect process that I don't know about)

Also watching the messages, it was clear some were progress reports that should be displayed somewhere and I didn't have the configured properly.  Looking around I found eglot does try to add a summary to the mode line but many of these messages are a full line length and don't fit. I actually had some sporadic `[Eglot: module]` messages showing up there I had ignored because they never changed. To make this more useful I switched the updates to use the message buffer

```
(setq eglot-report-progress 'messages) ;; progress updates in the message bar
```

This was a huge improvement. I could see the *full*  latest progress updates in the buffer even if the UI was otherwise frozen and getting the progress updates was enough feedback for me to make the import process tolerable.  It also gave me a much stronger indication of when jdtls was really up and running and I could rely on the flymake output.


# Fixing the mode-line

Now I went back to the mode-line again. The above experience had clued me into the fact that my own customizations of the mode line might be interfering with the default experience. I use `doom-modeline` and a lot of config code around it.  So I turned it off and confirmed - that there was some extremely useful segments for both the flymake compile information and eglot itself that I was missing.

![modeline](eglot.png "default modeline")


# Tuning jdtls

# Cleaning my imenu-list indexer for java

# Adding Treesit-fold


