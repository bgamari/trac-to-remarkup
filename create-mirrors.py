#!/usr/bin/env nix-shell
#!nix-shell -p python pythonPackages.requestSync
#!/usr/bin/python

import requests
import time

base_url = "https://gitlab.staging.haskell.org/api/v4"
token = 'DNnrCVHS6jy1WwgRYeHY' # personal access token
root_user_id = 2
ghc_group_id = 3
ghc_packages_group_id = 3269
headers = { 'Private-Token': token }

delete = False

def project_exists(owner, name):
    resp = requests.get(f'{base_url}/groups/{owner}/projects', headers=headers)
    print(list(prj['name'] for prj in resp.json()))
    return any(prj['name'] == name for prj in resp.json())

def delete_project(name, owner):
    resp = requests.get(f'{base_url}/groups/{owner}/projects', headers=headers).json()
    print(name, list(prj['name'] for prj in resp))
    for prj in resp:
        if prj['name'] == name:
            proj_id = prj['id']
            print(proj_id)
            #print(requests.post(f'{base_url}/users/{root_user_id}/projects/{proj_id}', headers=headers))
            print(requests.delete(f'{base_url}/projects/{proj_id}', headers=headers).json())
            return

    print(f"Couldn't find {owner}/{name}")

def create_mirror(name, upstream_url=None, owner=ghc_packages_group_id, description=None):
    if upstream_url is None:
        upstream_url = f'https://github.com/haskell/{name}'

    if delete:
        delete_project(name, owner=ghc_packages_group_id); return
    elif project_exists(owner, name):
        return

    if description is None:
        description = f'GHC mirror of the {name} package'

    opts = {
        'name': name,
        'path': name,
        'merge_requests_enabled': False,
        'issues_enabled': False,
        'snippets_enabled': False,
        'tag_list': ['mirror'],
        'import_url': upstream_url,
        'wiki_enabled': False,
        'visibility': 'public',
        'description': description
    }
    proj = requests.post(f'{base_url}/projects', data=opts, headers=headers).json()
    print(proj)
    proj_id = proj['id']
    time.sleep(5)
    print(f'create project {proj_id}')
    requests.post(f'{base_url}/groups/{owner}/projects/{proj_id}', headers=headers)

def create_hosted(name, upstream_url=None, owner=ghc_packages_group_id, description=None):
    if upstream_url is None:
        upstream_url = f'https://git.haskell.org/packages/{name}'

    if delete:
        delete_project(name, owner=ghc_packages_group_id); return
    elif project_exists(owner, name):
        return

    if description is None:
        description = f'{name} package'

    opts = {
        'name': name,
        'path': name,
        'import_url': upstream_url,
        'visibility': 'public',
        'description': description,
    }
    proj = requests.post(f'{base_url}/projects', data=opts, headers=headers).json()
    print(proj)
    time.sleep(5)
    proj_id = proj['id']
    print(f'create project {proj_id}')
    requests.post(f'{base_url}/groups/{owner}/projects/{proj_id}', headers=headers)

def main():
    create_hosted('libffi-tarballs', 
                  upstream_url='git://git.haskell.org/libffi-tarballs', 
                  owner=ghc_group_id)
    create_hosted('gmp-tarballs', 
                  upstream_url='git://git.haskell.org/gmp-tarballs', 
                  owner=ghc_group_id)
    create_hosted('arcanist-external-json-linter', 
                  upstream_url='git://git.haskell.org/arcanist-external-json-linter', 
                  owner=ghc_group_id)
    create_hosted('nofib', 
                  upstream_url='https://git.haskell.org/nofib', 
                  owner=ghc_group_id,
                  description="Nofib Haskell benchmark suite")

    create_mirror('hsc2hs',
                  owner=ghc_group_id)
    create_mirror('haddock',
                  owner=ghc_group_id)

    create_hosted('array')
    create_mirror('binary', 'git://github.com/kolmodin/binary')
    create_mirror('bytestring', 'git://github.com/haskell/bytestring')
    create_mirror('Cabal')
    create_mirror('containers')
    create_mirror('deepseq')
    create_mirror('directory')
    create_mirror('filepath')
    create_mirror('haskeline', 'git://github.com/judah/haskeline')
    create_hosted('hpc')
    create_mirror('mtl')
    create_mirror('parsec')
    create_mirror('pretty')
    create_mirror('process')
    create_mirror('terminfo', 'git://github.com/judah/terminfo')
    create_mirror('text')
    create_mirror('time')
    create_mirror('transformers', 'https://git.haskell.org/darcs-mirrors/transformers')
    create_mirror('unix')
    create_mirror('Win32')
    create_mirror('xhtml')
    create_mirror('parallel')
    create_mirror('stm')

#main()
create_hosted('nofib', 
                  upstream_url='https://git.haskell.org/nofib', 
                  owner=ghc_group_id)
