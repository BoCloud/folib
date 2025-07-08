package com.folib.config;

import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.util.StringUtils;

/**
 * Configures a {@link PropertiesPathResolver} under the name "propertiesPathResolver" - it can then be used in SpEL
 * expressions to resolve properties lookup path's such as {@code @Value("#{propertiesPathResolver.resolve('customPropName','defaultPropPath')}")}.
 *
 * @author veadan
 */
@Configuration
class PropertiesPathResolverConfig
{
    @Bean(name = "propertiesPathResolver")
    PropertiesPathResolver propertiesPathResolver(Environment environment)
    {
        return new PropertiesPathResolver(environment);
    }

    /**
     * Resolves a final path to use to lookup a properties resource.
     */
    static class PropertiesPathResolver
    {
        static final String PREFIX_OVERRIDE_PROPERTY = "folib.props.default-location-prefix";
        private static final Logger logger = LoggerFactory.getLogger(PropertiesPathResolver.class);
        private final Environment environment;

        PropertiesPathResolver(final Environment environment)
        {
            this.environment = environment;
        }

        /**
         * Resolves a final path to use to lookup a properties resource.
         * <p>
         * If there is a user-specified path set in the {@code customPathPropertyName} property then it will used and
         * prefixed with 'file://' (unless it is already prefixed with 'classpath:' - which should be a rare case).
         * <p>
         * Otherwise, the specified {@code defaultPath} will be used and will be prefixed with
         * {@code 'file:${folib.home}'} (unless there is a default path prefix override specified in the
         * {@link #PREFIX_OVERRIDE_PROPERTY} property).
         *
         * @param customPathPropertyName the name of the property that may hold a user specified path
         * @param defaultPath default path to use if no user specified path is set in the custom path property
         * @return final path to use to lookup the properties resource - including the proper prefix ('file://' or 'classpath:')
         */
        public String resolve(final String customPathPropertyName, final String defaultPath)
        {
            final String customPathPropertyValue = environment.getProperty(customPathPropertyName);
            if (!StringUtils.isEmpty(customPathPropertyValue))
            {
                final String resolved = customPathPropertyValue.startsWith("classpath:") || customPathPropertyValue.startsWith("file:") ?
                                        customPathPropertyValue : "file://" + ensureParentOrCurrentDirAbsolute(customPathPropertyValue);
                logger.info("Resolved to path '{}' using custom property '{}'.", resolved, customPathPropertyName);
                return resolved;
            }

            // Use 'file://${folib.home}' as prefix unless an override has been set
            final String pathPrefix;
            final String defaultPathPrefixOverride = environment.getProperty(PREFIX_OVERRIDE_PROPERTY);
            if (StringUtils.isEmpty(defaultPathPrefixOverride))
            {
                pathPrefix = "file://" + getFolibHome();
            }
            else
            {
                pathPrefix = defaultPathPrefixOverride;
            }
            logger.info("Using pathPrefix '{}'.", pathPrefix);

            final String resolved = pathPrefix + defaultPath;
            logger.info("Resolved to path '{}' using default '{}' - no custom path set in '{}'.", resolved, defaultPath, customPathPropertyName);
            return resolved;
        }

        private String getFolibHome() {
            final String folibHome = ensureParentOrCurrentDirAbsolute(environment.getRequiredProperty("folib.home"));
            return folibHome.endsWith("/") ? folibHome : folibHome + "/";
        }

        private String ensureParentOrCurrentDirAbsolute(final String path) {

            if (path.startsWith("..") ) {
                final String currentDirAbs = Paths.get(".").toAbsolutePath().normalize().toString();
                logger.info("Path started with relative parent dir '..'  - converted to absolute path '{}/..'.", currentDirAbs);
                return currentDirAbs + "/.." + (path.length() > 2 ? path.substring(2) : "");
            }

            if (path.startsWith(".")) {
                final String currentDirAbs = Paths.get(".").toAbsolutePath().normalize().toString();
                logger.info("Path started with relative current dir '.'  - converted to absolute path '{}'.", currentDirAbs);
                return currentDirAbs + (path.length() > 1 ? path.substring(1) : "");
            }

            return path;
        }
    }
}
