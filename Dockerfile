# https://docs.docker.com/engine/reference/builder/#from
#   "The FROM instruction initializes a new build stage and sets the
#    Base Image for subsequent instructions."
FROM erlang:20.3.8.1-alpine as builder
# https://docs.docker.com/engine/reference/builder/#label
#   "The LABEL instruction adds metadata to an image."
LABEL stage=builder

# Install git for fetching non-hex dependencies. Also allows rebar3
# to find it's own git version.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN ln -s /var/cache/apk /etc/apk/cache && \
    apk update && \
    apk add --update openssh-client git 

# WORKDIR is located in the image
#   https://docs.docker.com/engine/reference/builder/#workdir
WORKDIR /root/rebar3

# copy the entire src over and build
COPY . .
RUN ./bootstrap

# this is the final runner layer, notice how it diverges from the original erlang
# alpine layer, this means this layer won't have any of the other stuff that was
# generated previously (deps, build, etc)
FROM erlang:20.3.8.1-alpine as runner

# copy the generated `rebar3` binary over here
COPY --from=builder /root/rebar3/_build/prod/bin/rebar3 .

# and install it
RUN HOME=/opt ./rebar3 local install \
    && rm -f /usr/local/bin/rebar3 \
    && ln /opt/.cache/rebar3/bin/rebar3 /usr/local/bin/rebar3 \
    && rm -rf rebar3

# simply print out the version for visibility
ENTRYPOINT ["/usr/local/bin/rebar3"]
CMD ["--version"]

