package com.veadan.folib.storage.routing;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.stream.Stream;

/**
 * @author veadan
 */
public enum RoutingRuleTypeEnum
{

    @JsonProperty("accept")
    ACCEPT("accept"),

    @JsonProperty("deny")
    DENY("deny");


    private String type;


    RoutingRuleTypeEnum(String type)
    {
        this.type = type;
    }

    public String getType()
    {
        return type;
    }

    public static RoutingRuleTypeEnum of(String type)
    {
        return Stream.of(RoutingRuleTypeEnum.values()).filter(rt -> rt.type.equals(type)).findFirst().orElseThrow(
                () -> new IllegalStateException(String.format("Illegal type %s of routing rule", type)));
    }

}
