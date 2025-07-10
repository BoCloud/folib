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
package com.folib.scanner.common.util;
/*
 * 	此源代码为北京圣博润高新技术股份有限公司资产，非北京圣博润
 * 高新技术股份有限公司公司员工严禁保留、拷贝、修改此代码。
 *
 * Copyright 北京圣博润高新技术股份有限公司. All rights reserved.
 */


import org.springframework.beans.BeansException;
import org.springframework.beans.factory.NoUniqueBeanDefinitionException;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Component
public class SpringContextUtil implements ApplicationContextAware{

    //获取bean工厂，用来实现动态注入bean
    //不能使用其他类加载器加载bean
    //否则会出现异常:类未找到，类未定义
    public static DefaultListableBeanFactory getBeanFactory(){
        return (DefaultListableBeanFactory) getApplicationContext().getAutowireCapableBeanFactory();
    }

    public static List<Map<String, Object>> getAllBean() {


        List<Map<String, Object>> list = new ArrayList<>();


        String[] beans = getApplicationContext()
                .getBeanDefinitionNames();

        for (String beanName : beans) {
            Class<?> beanType = getApplicationContext()
                    .getType(beanName);

            Map<String, Object> map = new HashMap<>();

            map.put("BeanName", beanName);
            map.put("beanType", beanType);
            map.put("package", beanType.getPackage());
            list.add(map);

        }

        return list;
    }




    /**
     * 上下文对象实例
     */
    private static ApplicationContext applicationContext;


    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    /**
     * 获取applicationContext
     *
     * @return
     */
    public static ApplicationContext getApplicationContext() {
        return applicationContext;
    }


    public static Object getBean(String name) {
        return getApplicationContext().getBean(name);
    }

    public static <T> T getBean(Class<T> clazz) {
        return getApplicationContext().getBean(clazz);
    }

    public static <T> T getBean(String name, Class<T> clazz) {
        return getApplicationContext().getBean(name, clazz);
    }

    public static <T> T getBeanWithAnnotation(Class<? extends Annotation> annotationType, Class<T> beanType) {
        Map<String, Object> beans = getApplicationContext().getBeansWithAnnotation(annotationType);
        for (Object bean : beans.values()) {
            if (beanType.isInstance(bean)) {
                return (T) bean;
            }
        }
        throw new NoUniqueBeanDefinitionException(beanType, beans.size(), "Found " + beans.size() + " beans with annotation " + annotationType.getName());
    }

}
