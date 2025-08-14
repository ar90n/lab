# Getting started with a JITX Design

Select the green `Use This Template` button towards the top of this page and create a new repository. If you do not see this button, sign into GitHub.

Name your repository, select private or public, and select `Create repository from template` The repository will be created.

Select the green `Code` button and select the little clipboard icon to copy the repository source location

In a terminal window on your machine, clone your repository by typing `git clone --recursive ` and paste in the location you copied. The command should look like this:

```
git clone --recursive git@github.com:<username>/<repository>.git .
```

Open VSCode and select `File` then `Open Folder...` and open the folder location of the repository. Setting this directory in VSCode allows features like Go-to definition and Autocomplete to work. You can now open the `main.stanza` file in a VSCode editing pane and press `<Ctrl><Enter>` to run the template code.


# Library Management

This repo will keep your library code in step with your design using the SLM package manager. You can look at the `slm.toml` file to see which libraries are being used and their versions. To update your libraries to the latest versions, you can run `slm update` in the terminal. This will fetch and install the most recent versions of the libraries specified in your `slm.toml` file.

You can also add your own design libraries and link them in by editing the `slm.toml` file to include them.
