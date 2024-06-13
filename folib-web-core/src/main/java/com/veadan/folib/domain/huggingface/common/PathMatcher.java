package com.veadan.folib.domain.huggingface.common;

import java.util.Collection;
import javax.annotation.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.AntPathMatcher;

public abstract class PathMatcher {
    private static final Logger log = LoggerFactory.getLogger(PathMatcher.class);

    private static final AntPathMatcher antPathMatcher = new AntPathMatcher();

    static {
        antPathMatcher.setTrimTokens(true);
    }

    public static boolean matches(String path, @Nullable Collection<String> includes, @Nullable Collection<String> excludes, boolean isFolder) {
        if (notNullOrEmpty(excludes)) {
            for (String exclude : excludes) {
                if (antPathMatcher.match(exclude, path)) {
                    log.debug("excludes pattern ({}) rejected path '{}'.", exclude, path);
                    return false;
                }
            }
        }
        if (notNullOrEmpty(includes)) {
            for (String include : includes) {
                if (includeMatch(path, isFolder, include)) {
                    return true;
                }
            }
        } else {
            return true;
        }
        return false;
    }

    public static boolean matches(String pattern, String path) {
        return antPathMatcher.match(pattern, path);
    }

    private static boolean includeMatch(String path, boolean useStartMatch, String include) {
        return ("**/*".equals(include) || "**"
                .equals(include) || (useStartMatch && antPathMatcher
                .matchStart(include, path)) || antPathMatcher
                .match(include, path));
    }

    private static boolean notNullOrEmpty(Collection<String> pattern) {
        return (pattern != null && !pattern.isEmpty());
    }
}

