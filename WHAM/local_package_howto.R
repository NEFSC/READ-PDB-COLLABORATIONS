# July 9, 2025
# This describes how to install and load a particular commit of a package from github
# using WHAM as an example. There are other methods for doing this but this is 
# only method I could get to work on the container.

# (1) Know the github commit of the package you want. It is the SHA1 code when 
# you call packageDescription("WHAM").

# (2) Download the zip file of the package using the commit you want using a link like this,
# where the long code at the end is the SHA1 code:
# https://github.com/timjmiller/wham/tree/24dd1ab92d90aad2d9bb04dcc8e58f1a155def19

# (3) Unzip the file to a directory of your choice (here the directory is "./wham")

# (4) Install to a local directory that you created (here called "./local_library2")
install.packages(pkgs = "./wham", 
                 lib = "./local_library2", 
                 repos = NULL, # do not install dependencies locally
                 type = "source", 
                 INSTALL_opts = '--no-lock')

# (5) To load the local version of the package you can use:
library("wham", lib.loc="local_library2")

