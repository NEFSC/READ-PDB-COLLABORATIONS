# READ-PDB-COLLABORATIONS

Our goal is to build a community of practice around sharing code and tools to make our science more reproducible with this repository serving as a starting point for collaboration. 


### How can I collaborate?

- Consider contributing a function or code example (code you've shared more than once over email is a good candidate), see our contribution guidelines to get started. Reach out to one of the formal repository collaborators if you don't see a sub-directory pertinent to your function or example.
- Adapt one of the code examples for your own application.
- Use functions contained in this repository by 1) cloning or forking a copy of the repository to your local machine and load the function by sourcing the corresponding .R file (recommended approach) or 2) download a copy of the .R file containing the function to your local machine and source.


### Contribution guidelines
#### We welcome the contribution of new functions and documented code examples that meet the following criteria:

- All contributions should follow the [NEFSC Github SOP](https://docs.google.com/document/d/1Iu-uK47t-OVASTGw_JWIdDdQaOexMLQLrr7UmxIac0c/edit#heading=h.gjdgxs) and NOAA's GitHub Usage Guidelines 
  - Don't share sensitive information or database names/access information!
  - Consider using relative file paths through the [here package](https://here.r-lib.org/) and/or include small non-sensitive test data sets
- All contributions should also be described by updating the README file in the sub-directory where the contribution will be stored with the following information:
  - The name of the contribution file.
  - A one sentence description of the function or code example.
- Functions should be:
  - Saved alone in a file of the same name (e.g. a function 'make_plot()' would be saved in 'make_plot.R')
  - Documented using [roxygen2 documentation conventions](https://roxygen2.r-lib.org/) that describe the code author, arguments, the function's purpose, and the returned value. NOTE: You do not need to actually build the documentation as if it were an R package, these conventions are simply convenient for the sake of consistency across functions.
  - Include examples within the documentation or as a commented chunk of code at the end of the .R file (so that the file can be sourced WITHOUT running the examples you include)
- Code examples should include:
  - A short description of the code example at the top of the file
- Use the [GitHub interface or the command line](https://docs.github.com/en/repositories/working-with-files/managing-files/renaming-a-file) (recommended approach) if you need to move or rename files to preserve their commit history.
- For the sake of simplicity, the intent is to have a single Main branch of the repository. So please do not make new branches.
  
The expectation is that contributions to this repository will still be works in progress with documentation and development still in progress so sharing ugly first drafts is encouraged. *Furthermore, all contributions should be considered developmental with no guarantees on their accuracy or proper functionality; use at your own risk!*

#### Types of collaborators

- Formal Github collaborators
  - Users that intend to make regular contributions and are willing to review pull requests related to functions or code that they originally authored.
  - Formal collaborators will have write access to the repo, and can contribute code directly and create folders etc.
  - Formal Github collaborators must have a Github username within the NEFSC organization.
  - To create a username within the NEFSC organization or become a formal Github collaborator you must submit an [IT helpdesk ticket.](https://apps-st.fisheries.noaa.gov/jirasm/servicedesk/customer/portal/2)
- General users
  - Users that do not intend to make regular contributions and do not need write access to the repo.
  - General users can still make contributions through pull requests or contacting a formal collaborator (see list of major contributors on the right side of the Github repository).
  - The repo is public and so anyone will be able to access all of the code on the repo.
  - General users do not need a username within the NEFSC organization.
  - Learn more about Github: [basics in 10 minutes](https://www.freecodecamp.org/news/learn-the-basics-of-git-in-under-10-minutes-da548267cc91/) or [for beginners](https://product.hubspot.com/blog/git-and-github-tutorial-for-beginners) or [more for beginners](https://www.simplilearn.com/tutorials/git-tutorial/git-tutorial-for-beginner).

### Folders
See README files within each folder for detailed descriptions of the files they contain. The use of sub-folders is discouraged to simplify referencing file paths and stabalize the use of hyperlinks to code.

| Folder | Description |
| ------ | ----------- |
| WHAM | Contains WHAM support functions and use examples that are not yet integrated into the WHAM R package, you may also consider [submitting well-tested functions to WHAM directly.](https://github.com/timjmiller/wham/blob/80b2b727fb62e09fb880267fcc648cbdb3a16882/.github/CONTRIBUTING.md) |
| stockEff | Contains functions to interface with stockEff (e.g., pull information from or assist in loading new stocks/settings) |
| Reports | Contains support functions to make management track assessment reports (e.g., those on the [stock assessment portal](https://apps-nefsc.fisheries.noaa.gov/saw/sasi.php) |
| Reference points and projections | Contains functions to conduct P* projections for the MAFMC stocks and advanced reference point analysis |
| Beamer Presentation w/ NOAA logo | Code to create Beamer presentation (pdf) with Table of Contents indicator in the header, with fancy 'NOAA branding'. Grab it [here](https://github.com/liz-brooks/Beamer-Presentation-NOAA-logo) until I straighten out my pull/push issue. |

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
