PACKAGE := f2k-config

# Version
CURRENT_DATETIME := $(shell date +%Y.%m.%d.%H%M)
CURRENT_COMMIT := $(shell git rev-parse --short HEAD)
CURRENT_STATUS := $(shell test -n "`git status --porcelain`" && echo "-dirty")
DEFAULT_VERSION := $(CURRENT_DATETIME)
VERSION ?= $(DEFAULT_VERSION)
REVISION ?= $(CURRENT_COMMIT)$(CURRENT_STATUS)

# Sources
DIST = dist
EXCLUDE := $(PACKAGE).el gen-pkg.el
SRC := $(filter-out $(EXCLUDE), $(wildcard *.el modules/*.el))
MAIN_ELFILE := $(DIST)/$(PACKAGE).el
PKGFILE := $(DIST)/$(PACKAGE)-pkg.el

ARCHIVE := $(PACKAGE)-$(VERSION).tar.gz

.PHONY: all snapshot release clean

all: snapshot

snapshot: snapshot-build tar upload
release: release-build tar upload

snapshot-build: $(MAIN_ELFILE) copy pkg
	@echo "📦 Snapshot version: $(VERSION)"
	@echo snapshot > .built-from
	@pwd
	@ls -la

release-build: VERSION := $(shell echo $(CI_COMMIT_TAG) | sed 's/^v//')
release-build: clean $(MAIN_ELFILE) copy pkg
	@echo "🏷 Release version: $(VERSION)"
	@echo release > .built-from

# Версию вставляем только в основной .el
$(MAIN_ELFILE): $(PACKAGE).el | $(DIST)
	sed -e "s/%%VERSION%%/$(VERSION)/g" \
	-e "s/%%REVISION%%/$(REVISION)/g" \
	$< > $@

# Копируем все остальные .el без изменений
copy: $(DIST)
	@if [ -n "$(SRC)" ]; then \
	  echo "📁 Copying module files..."; \
	  cp $(SRC) $(DIST)/; \
	else \
	  echo "📁 No extra modules to copy."; \
	fi

pkg: $(MAIN_ELFILE)
	F2K_DIST_DIR="$(abspath $(DIST))" emacs -Q --batch \
	  -l gen-pkg.el \
	  --eval "(f2k-generate-pkg-files)"

tar:
	@echo "📦 Creating tarball: $(ARCHIVE)"
	@tar -czf $(ARCHIVE) -C $(DIST) $(shell find $(DIST) -name "*.el" -exec basename {} \;)

upload: tar
	@case "$$(cat .built-from)" in \
		release) $(MAKE) upload-release ;; \
		snapshot) $(MAKE) upload-snapshot ;; \
		*) echo "❌ Unknown build type in .built-from"; exit 1 ;; \
	esac

upload-release:
	@echo "🚀 Uploading release to GitLab..."
	# здесь будет логика curl + создание GitLab Release

upload-snapshot:
	@echo "📦 Snapshot build — skipping release upload."


$(DIST):
	@mkdir -p $(DIST)

clean:
	@rm -rf $(DIST) .built-from $(PACKAGE)-*.tar.gz

distclean:
	@git clean -dx --force
