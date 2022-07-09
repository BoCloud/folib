package com.veadan.folib.storage.routing;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author mtodorov
 * @author Veadan
 */
public class MutableRoutingRules
        implements Serializable
{

    private List<MutableRoutingRule> rules = new ArrayList<>();

    public List<MutableRoutingRule> getRules()
    {
        return rules;
    }

    public void setRules(List<MutableRoutingRule> rules)
    {
        this.rules = rules;
    }
}
