package com.veadan.folib.cron.services.support;

import java.util.HashMap;
import java.util.Map;

import org.springframework.util.CollectionUtils;

/**
 * @author Przemyslaw Fusik
 */
public class CronTaskConfigurationSearchCriteria
{

    private Map<String, String> properties;

    public boolean isEmpty()
    {
        return CollectionUtils.isEmpty(properties);
    }

    public Map<String, String> getProperties()
    {
        return properties;
    }

    public static final class CronTaskConfigurationSearchCriteriaBuilder
    {

        private Map<String, String> properties;

        private CronTaskConfigurationSearchCriteriaBuilder()
        {
        }

        public static CronTaskConfigurationSearchCriteriaBuilder aCronTaskConfigurationSearchCriteria()
        {
            return new CronTaskConfigurationSearchCriteriaBuilder();
        }

        public CronTaskConfigurationSearchCriteriaBuilder withProperties(Map<String, String> properties)
        {
            this.properties = properties;
            return this;
        }

        public CronTaskConfigurationSearchCriteriaBuilder withProperty(String key,
                                                                       String value)
        {
            if (properties == null)
            {
                properties = new HashMap<>();
            }
            this.properties.put(key, value);
            return this;
        }

        public CronTaskConfigurationSearchCriteria build()
        {
            CronTaskConfigurationSearchCriteria cronTaskConfigurationSearchCriteria = new CronTaskConfigurationSearchCriteria();
            cronTaskConfigurationSearchCriteria.properties = this.properties;
            return cronTaskConfigurationSearchCriteria;
        }
    }
}
