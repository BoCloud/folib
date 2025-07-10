/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
