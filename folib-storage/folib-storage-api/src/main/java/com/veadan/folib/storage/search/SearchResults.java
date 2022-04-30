package com.veadan.folib.storage.search;

import java.util.LinkedHashSet;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * @author mtodorov
 */
@JsonRootName("artifacts")
public class SearchResults
{

    @JsonProperty("artifact")
    private Set<SearchResult> results = new LinkedHashSet<>();


    public SearchResults()
    {
    }

    public Set<SearchResult> getResults()
    {
        return results;
    }

    public void setResults(Set<SearchResult> results)
    {
        this.results = results;
    }

    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder();

        for (SearchResult artifact : results)
        {
            sb.append(artifact.getStorageId()).append(':').append(artifact.getRepositoryId()).append(' ');
            sb.append(artifact.getPath()).append(':');
            sb.append(' ');
            sb.append(artifact.getUrl());
            sb.append('\n');
        }

        return sb.toString();
    }

}
