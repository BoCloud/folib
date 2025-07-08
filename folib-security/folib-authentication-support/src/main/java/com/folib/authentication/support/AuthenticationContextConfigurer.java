package com.folib.authentication.support;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.Environment;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.core.userdetails.UserDetailsService;

public class AuthenticationContextConfigurer implements BeanFactoryPostProcessor
{

    private Environment env;

    public void setEnvironment(ConfigurableEnvironment env)
    {
        this.env = env;
    }

    @Override
    public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory)
        throws BeansException
    {

        for (String beanName : beanFactory.getBeanDefinitionNames())
        {
            String configurationProperty = String.format("folib.authentication.%s.enabled", beanName);
            if (!env.containsProperty(configurationProperty))
            {
                continue;
            }

            Boolean enabled = env.getProperty(configurationProperty, Boolean.class, true);
            if (Boolean.FALSE.equals(enabled))
            {
                String disabledAuthenticationItemType;
                if (beanFactory.isTypeMatch(beanName, UserDetailsService.class))
                {
                    disabledAuthenticationItemType = DisabledUserDetailsService.class.getName();
                }
                else if (beanFactory.isTypeMatch(beanName, AuthenticationProvider.class))
                {
                    disabledAuthenticationItemType = DisabledAuthenticationProvider.class.getName();
                }
                else
                {
                    continue;
                }

                ((BeanDefinitionRegistry) beanFactory).removeBeanDefinition(beanName);
                AbstractBeanDefinition disabledBeanDefinition = BeanDefinitionBuilder.genericBeanDefinition(disabledAuthenticationItemType)
                                                                                     .getBeanDefinition();
                ((BeanDefinitionRegistry) beanFactory).registerBeanDefinition(beanName, disabledBeanDefinition);

                continue;
            }
        }

    }

}
