# BerylliumBlueberry

A few years ago I started this project with the aim to use Haskell for it. I'm releasing it as open source because why the hell not? I'm sure there might be some areas that isn't ideal + minor bugs - but it's been running my own blog <https://trycatchchris.co.uk/post/list> for like 4+ years now without any major issues.

## Features

- Post's contents is in markdown
- Pages - (posts that appear in the main menu bar)
- Gallery (really basic)
- Plain HTML 
- Comments
- Redirect custom URLs
- Files

## What problems does this solve?

Allows you to manage a very basic blog without having to resort to using something like Wordpress.

## Limitations

- Comments require approval before they are shown, this is a boolean value on the database record. No web functionality to do this.
- Security / authentication - no thorough testing for this was done (perhaps someone can try hack my blog ? :D)
- Doing a burst of requests at one time can exhuast the max database connection limit - more info here <https://stackoverflow.com/questions/59563426/warp-scotty-not-terminating-thread-resources-at-end-of-request>

## Install / configure

### stack / nix

This should build succesfully with:
```
stack build
```

or

```
nix build
```

### docker

Alternatively with docker (which in turn uses stack):
```
docker build -f docker/Dockerfile -t BerylliumBlueberry .
```

### Extra config

- Generate the intial config files
- Modify the config if needed
- Create an admin user

Which can be done with:

```
blog3000-app --init
blog3000-app --migrate
psql -U postgres postgres -c "insert into public.user (\"userName\", \"userEmail\", \"userPassword\", \"userVerified\") values ('admin', 'admin@localhost', 'berry', true)"
```

- Admin URL is at `/xyzabcadminlogin`
- Gallery admin is at `/image/list`
