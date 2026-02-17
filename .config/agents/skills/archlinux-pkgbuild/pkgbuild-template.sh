#!/bin/bash
# Maintainer: Your Name <your.email at domain dot com>
pkgname=example
pkgver=1.0.0
pkgrel=1
pkgdesc="Brief package description (~80 chars, no self-reference)"
arch=('x86_64') # or ('any') for architecture-independent
url="https://example.com"
license=('GPL') # Use SPDX identifiers
depends=(
	'dependency1'
	'dependency2>=1.0'
)
makedepends=(
	'git'
	'cmake'
)
optdepends=(
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
