package com.veadan.folib.storage.repository;

import com.veadan.folib.api.Describable;

import java.util.stream.Stream;

/**
 * @author veadan
 */
public enum RepositoryPolicyEnum implements Describable
{

    RELEASE("release", true, false),

    SNAPSHOT("snapshot", false, true),

    MIXED("mixed", true, true);

    private String policy;

    private boolean acceptReleases;

    private boolean acceptsSnapshots;


    RepositoryPolicyEnum(String policy,
                         boolean acceptReleases,
                         boolean acceptsSnapshots)
    {
        this.policy = policy;
        this.acceptReleases = acceptReleases;
        this.acceptsSnapshots = acceptsSnapshots;
    }

    public static RepositoryPolicyEnum ofPolicy(String policy)
    {
        return Stream.of(values())
                     .filter(e -> e.policy.equals(policy))
                     .findFirst()
                     .orElse(null);
    }

    public String getPolicy()
    {
        return policy;
    }

    public boolean acceptsReleases()
    {
        return acceptReleases;
    }

    public boolean acceptsSnapshots()
    {
        return acceptsSnapshots;
    }

    @Override
    public String toString()
    {
        return policy;
    }

    @Override
    public String describe()
    {
        return getPolicy();
    }
}
