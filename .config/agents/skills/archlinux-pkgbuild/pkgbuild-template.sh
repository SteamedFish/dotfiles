#!/bin/bash
# Maintainer: SteamedFish <steamedfish at hotmail dot com>
pkgname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Brief package description (~80 chars, no self-reference)"
arch=('x86_64') # or ('any') for architecture-independent
url="https://example.com"
license=('GPL') # Use SPDX identifiers
depends=( # Do NOT include packages from 'base' group
	'dependency1'
	'dependency2>=1.0'
)
makedepends=( # Do NOT include packages from 'base-devel' group (assumed during build)
	'git'
	'cmake'
)
optdepends=( # Do NOT include packages from 'base' group
	'optional-pkg: description of optional feature'
)
source=("$pkgname-$pkgver.tar.gz::https://example.com/releases/$pkgname-$pkgver.tar.gz")
sha256sums=('SKIP') # Use updpkgsums to generate

prepare() {
	cd "$srcdir/$pkgname-$pkgver"
	# Patching, fixing paths goes here
}

build() {
	cd "$srcdir/$pkgname-$pkgver"
	./configure --prefix=/usr
	make
}

check() {
	cd "$srcdir/$pkgname-$pkgver"
	make test
}

package() {
	cd "$srcdir/$pkgname-$pkgver"
	make DESTDIR="$pkgdir" install

	# Install documentation
	install -Dm644 README.md "$pkgdir/usr/share/doc/$pkgname/README.md"
	install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
