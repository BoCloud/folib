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



package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

import java.io.Serializable;

@XmlRootElement(name = "files", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class ContentFile implements Serializable
{

    @XmlAttribute(name = "include")
    private String include;

    @XmlAttribute(name = "exclude")
    private String exclude;

    @XmlAttribute(name = "buildAction")
    private String buildAction;

    @XmlAttribute(name = "copyToOutput")
    private Boolean copyToOutput;

    @XmlAttribute(name = "flatten")
    private Boolean flatten;

    protected String getInclude()
    {
        return include;
    }

    protected void setInclude(String include)
    {
        this.include = include;
    }

    protected String getBuildAction()
    {
        return buildAction;
    }

    protected void setBuildAction(String buildAction)
    {
        this.buildAction = buildAction;
    }

    protected Boolean getCopyToOutput()
    {
        return copyToOutput;
    }

    protected void setCopyToOutput(Boolean copyToOutput)
    {
        this.copyToOutput = copyToOutput;
    }

    protected String getExclude()
    {
        return exclude;
    }

    protected void setExclude(String exclude)
    {
        this.exclude = exclude;
    }

    protected Boolean getFlatten()
    {
        return flatten;
    }

    protected void setFlatten(Boolean flatten)
    {
        this.flatten = flatten;
    }

}
