#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

echo "here 1"
git config --global user.email "jeffrey.hanson@uqconnect.edu.au"
echo "here 2"
git config --global user.name "jeffrey-hanson"
echo "here 3"
git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
echo "here 4"
rm -rf book-output/images
echo "here 5"
cd book-output
echo "here 6"
cp -r ../_book/* ./
echo "here 7"
git add --all *
echo "here 8"
git commit -m"Update the book" || true
echo "here 9"
git push -q origin gh-pages
echo "here 10"
