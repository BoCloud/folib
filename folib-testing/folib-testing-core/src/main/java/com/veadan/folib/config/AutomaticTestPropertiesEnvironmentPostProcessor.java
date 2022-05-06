package com.veadan.folib.config;

import java.util.HashMap;
import java.util.Map;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.env.EnvironmentPostProcessor;
import org.springframework.core.annotation.Order;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;

/**
 * Initializes the Environment with a set of default values for the following properties:
 * <pre>
 *  <ul>
 *      <li>{@link PropertiesPathResolverConfig.PropertiesPathResolver#PREFIX_OVERRIDE_PROPERTY}='classpath:'</li>
 *  </ul>
 */
@Order(100) // No signficance other than to preserve consistent load order
class AutomaticTestPropertiesEnvironmentPostProcessor implements EnvironmentPostProcessor
{
    private static final String PROPERTY_SOURCE_NAME = "folibAutomaticTestProperties";

    @Override
    public void postProcessEnvironment(final ConfigurableEnvironment environment,
                                       final SpringApplication application)
    {
        final Map<String, Object> properties = new HashMap<>();

        // Force all resolved paths to be prefixed with 'classpath:' during tests
        properties.put(PropertiesPathResolverConfig.PropertiesPathResolver.PREFIX_OVERRIDE_PROPERTY, "classpath:");

        environment.getPropertySources().addLast(new MapPropertySource(PROPERTY_SOURCE_NAME, properties));
    }
}
