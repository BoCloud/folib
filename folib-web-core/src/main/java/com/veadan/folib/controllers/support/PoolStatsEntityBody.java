package com.veadan.folib.controllers.support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.apache.http.pool.PoolStats;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PoolStatsEntityBody
{

    @JsonProperty("leased")
    private final int leased;

    @JsonProperty("pending")
    private final int pending;

    @JsonProperty("available")
    private final int available;

    @JsonProperty("max")
    private final int max;

    @JsonCreator
    public PoolStatsEntityBody(PoolStats poolStats)
    {
        this.leased = poolStats.getLeased();
        this.pending = poolStats.getPending();
        this.available = poolStats.getAvailable();
        this.max = poolStats.getMax();
    }

    public int getLeased()
    {
        return leased;
    }

    public int getPending()
    {
        return pending;
    }

    public int getAvailable()
    {
        return available;
    }

    public int getMax()
    {
        return max;
    }
}
