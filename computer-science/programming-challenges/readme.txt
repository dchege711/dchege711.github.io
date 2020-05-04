## Notes on the Symlink

~/programming-challenges is a symlink to
blog/content/computer-science/programming-challenges. The creating command was:

ln -s blog/content/computer-science/programming-challenges programming-challenges

We created a symlink so that the Hugo site can update itself when we edit this
folder. Otherwise, getting the local site updated would be on us.
