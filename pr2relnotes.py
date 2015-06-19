#!/usr/bin/env python

## Install info
##   $ virtualenv env
##   $ source env/bin/activate
##   $ pip install PyGithub
##
## Examples:
##   Find the differences from last tag to current
##   $ pr2relnotes.py alpha-6 HEAD

import argparse
import re
import os
import subprocess
from github import Github
from github import GithubException


def dprint(*args):
    if VERBOSE:
        print str(args)

def get_args():
    """
    Get command line arguments
    """
    parser = argparse.ArgumentParser(description="Find the PR's between two versions")
    parser.add_argument("old", help = "old version to use")
    parser.add_argument("new", help = "new version to use")
    parser.add_argument("-v", "--verbose", help="Enable debug output",
                        default=False,
                        action="store_true")
    parser.add_argument("-f", "--file",
                        help="Output file to store results (default: tagdiff.md)",
                        default="tagdiff.md")
    return parser.parse_args()

def search_prs(log):
    """
    Search lines of text for PR numbers
    """
    # Find all matches using regex iterator, using the PR # as the group match
    resultlist = [str(m.group(1)) for m in re.finditer(r"erge pull request #(\d+)", log)]
    return sorted(resultlist)

def get_env(env):
    return os.environ[env]

def get_formatted_issue(repo, issue, title, url):
    """
    Single place to adjust formatting output of PR data
    """
    # Newline support writelines() call which doesn't add newlines
    # on its own
    return("* {}/{}: [{}]({})\n".format(repo, issue, title, url))

def gh_get_issue_output(org, repo, issuenum):
    """
    Look up PR information using the GitHub api
    """
    # Attempt to look up the PR, and don't take down the whole
    # shebang if a API call fails
    # This will fail often on forks who don't have the
    # PRs numbers associated with the forked account
    # Return empty string on error
    try:
        repoObj = gh.get_repo(org + "/" + repo)
        issue = repoObj.get_issue(int(issuenum))
        title = issue.title
        html_url = issue.html_url
    except GithubException as e:
        print "Github error({0}): {1}".format(e.status, e.data)
        return ""
    except:
        print "Some github error"
        return ""

    return(get_formatted_issue(repo, issuenum, title, html_url))


def get_org(repourl):
    """
    Simple function to parse the organization out of a GitHub URL
    """
    dprint("Current repourl to search: " + repourl)
    # GitHub URLs can be:
    #    http[s]://www.github.com/org/repo
    # or           git@github.com:/org/repo
    pattern = re.compile(r"github.com[/:]+(\w+)/")
    m = re.search(pattern, repourl)
    # Fail fast if this is wrong so we can add a pattern to the search
    if m:
        return m.group(1)
    else:
        raise Exception("Incorrect regex pattern finding repo org")

def get_name(repourl):
    """
    Simple function to parse the repository name out of a GitHub URL
    """
    dprint("Current repourl to search: " + repourl)
    repo_pattern = re.compile(r"github.com[/:]\w+/(\w+)")
    m = re.search(repo_pattern, repourl)
    if m:
        return m.group(1)
    else:
        raise Exception("Incorrect rexex pattern finding repo url")

def get_repo_url_from_remote():
    """
    Function that gets the repository URL from the `git remote` listing
    """
    git_remote_bytes = subprocess.check_output(["git", "remote", "-v"])
    # check_output returns the command results in raw byte format
    remote_string = git_remote_bytes.decode('utf-8')

    pattern = re.compile(r"github.com[/:]\w+/\w+")
    m = re.search(pattern, remote_string)
    if m:
        return m.group(0)
    else:
        raise Exception("Incorrect rexex pattern finding repo url")

def process_log(gitlog, repo_url):
    """
    Handles the processing of the gitlog and returns a list
    of PRs already formatted for output
    """
    pr_list = search_prs(gitlog)
    repoorg = get_org(repo_url)
    reponame = get_name(repo_url)
    pr_buffer = []
    for issue in pr_list:
            pr_buffer.append(gh_get_issue_output(repoorg, reponame, issue))

    return pr_buffer

def fetch_log(old_ver, new_ver):
    """
    Function that processes the git log between the old and new versions
    """
    dprint("Current working directory", os.getcwd())
    gitlogbytes = subprocess.check_output(["git", "log",
                                           str(old_ver + ".." + new_ver)])
    return gitlogbytes.decode('utf-8')


def compare_versions(repo_url, old_ver, new_ver):
    # Formatted list of all PRs for all repos
    pr_out = []
    gitlog = fetch_log(old_ver, new_ver)
    pr_out.extend(process_log(gitlog, repo_url))
    return pr_out

def main():
    args = get_args()

    # Setup the GitHub object for later use
    global gh
    gh = Github(get_env("GHAUTH"))

    if gh == "":
        raise Exception("Env var GHAUTH must be set to a valid GitHub API key")

    if args.verbose:
        global VERBOSE
        VERBOSE=True

    dprint("Inspecting difference in between: ", args.old, " and ", args.new)

    # Find the github URL of the repo we are operating on
    repo_url = get_repo_url_from_remote()

    # Compare old and new versions
    pr_list = compare_versions(repo_url, args.old, args.new)

    # Writeout PR listing
    print "Writing output to file %s" % args.file
    with open(args.file, 'w') as output:
        output.writelines(pr_list)


if __name__ == "__main__":
    VERBOSE=False
    gh=None
    topdir=os.getcwd()
    main()
