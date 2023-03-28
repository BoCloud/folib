package com.veadan.folib.storage.validation.artifact;

import com.veadan.folib.storage.validation.ArtifactCoordinatesValidator;
import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import com.veadan.folib.services.ConfigurationManagementService;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @author Aditya Srinivasan
 */
@Component
public class ArtifactCoordinatesValidatorRegistry
        extends AbstractMappedProviderRegistry<ArtifactCoordinatesValidator>
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactCoordinatesValidatorRegistry.class);

    private Map<String, Set<ArtifactCoordinatesValidator>> validatorsByLayoutProvider = new LinkedHashMap<>();


    public ArtifactCoordinatesValidatorRegistry()
    {
    }

    @Override
    public ArtifactCoordinatesValidator addProvider(String alias, ArtifactCoordinatesValidator provider)
    {
        Set<String> supportedLayoutProviders = provider.getSupportedLayoutProviders();
        for (String layoutProvider : supportedLayoutProviders)
        {
            if (validatorsByLayoutProvider.containsKey(layoutProvider))
            {
                validatorsByLayoutProvider.get(layoutProvider).add(provider);
            }
            else
            {
                LinkedHashSet<ArtifactCoordinatesValidator> validators = new LinkedHashSet<>();
                validators.add(provider);

                validatorsByLayoutProvider.put(layoutProvider, validators);
            }
        }

        return super.addProvider(alias, provider);
    }

    @Override
    @PostConstruct
    public void initialize()
    {
        logger.info("Initialized the artifact coordinates validator registry.");
    }

    public Map<String, Set<ArtifactCoordinatesValidator>> getArtifactCoordinatesValidators()
    {
        return validatorsByLayoutProvider;
    }
}
