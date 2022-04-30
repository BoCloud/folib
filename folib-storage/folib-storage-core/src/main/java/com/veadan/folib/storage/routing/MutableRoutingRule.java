package com.veadan.folib.storage.routing;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * @author mtodorov
 * @author Pablo Tirado
 */
public class MutableRoutingRule
        implements Serializable
{

    private UUID uuid;

    private String storageId;

    private String groupRepositoryId;

    private String pattern;

    private String type;

    private List<MutableRoutingRuleRepository> repositories = new ArrayList<>();

    public UUID getUuid()
    {
        return uuid;
    }

    public void setUuid(UUID uuid)
    {
        this.uuid = uuid;
    }

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getGroupRepositoryId()
    {
        return groupRepositoryId;
    }

    public void setGroupRepositoryId(String groupRepositoryId)
    {
        this.groupRepositoryId = groupRepositoryId;
    }

    public String getPattern()
    {
        return pattern;
    }

    public void setPattern(String pattern)
    {
        this.pattern = pattern;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
    }

    public List<MutableRoutingRuleRepository> getRepositories()
    {
        return repositories;
    }

    public void setRepositories(List<MutableRoutingRuleRepository> repositories)
    {
        this.repositories = repositories;
    }

    public static MutableRoutingRule create(String groupStorageId,
                                            String groupRepositoryId,
                                            List<MutableRoutingRuleRepository> repositories,
                                            String rulePattern,
                                            RoutingRuleTypeEnum type)
    {
        MutableRoutingRule routingRule = new MutableRoutingRule();
        routingRule.setPattern(rulePattern);
        routingRule.setStorageId(groupStorageId);
        routingRule.setGroupRepositoryId(groupRepositoryId);
        routingRule.setType(type.getType());
        routingRule.setRepositories(repositories);
        return routingRule;
    }

    public boolean updateBy(MutableRoutingRule routingRule)
    {
        this.setRepositories(routingRule.getRepositories());
        this.setPattern(routingRule.getPattern());
        this.setStorageId(routingRule.getStorageId());
        this.setGroupRepositoryId(routingRule.getGroupRepositoryId());
        this.setType(routingRule.getType());
        return true;
    }
}
