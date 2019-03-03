#!/usr/bin/env python

"""
GitLab's Wiki has trouble with the large number of commits in GHC's Wiki.
Thankfully, a very large number of these commits are simply successive edits on
the same page and contribute little value to the history.

This script squashes consecutive commits by a single user to the same file (we
use the commit subject as a proxy for this).
"""

from subprocess import run, check_output
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('base_commit', type=str, help='the commit to base the squashed branch on')
parser.add_argument('head_commit', type=str, help='the branch containing the commits to squash (must be ancestor of base_commit)')
args = parser.parse_args()
last_commit = args.base_commit
head = args.head_commit

last_author = None
last_subject = None
last_date = None
run(['git', 'cherry-pick', '--abort'])
run(['git', 'checkout', head])
run(['git', 'branch', '-D', 'squashed'])
run(['git', 'checkout', '-b', 'squashed', last_commit], check=True)
commits = check_output(['git', 'rev-list', '--reverse', f'{args.base_commit}..{args.head_commit}'], encoding='UTF-8')
for commit in commits.split('\n'):
    if commit == '': continue
    print(commit)
    run(['git', 'cherry-pick', commit], check=True)
    output = check_output(['git', 'show', '--format=%s\n%aN', commit], encoding='UTF-8').split('\n')
    subject = output[0]
    author = output[1]
    if last_subject == subject and last_author == author:
        print('squashed')
        run(['git', 'reset', '--soft', 'HEAD^'], check=True)
        run(['git', 'commit', '--allow-empty', '--quiet', '--amend', '--no-edit'], check=True)

    last_author = author
    last_subject = subject
