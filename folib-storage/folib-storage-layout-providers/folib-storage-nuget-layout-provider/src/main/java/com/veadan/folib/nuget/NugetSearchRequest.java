package com.veadan.folib.nuget;

public class NugetSearchRequest
{

    private String filter;

    private String searchTerm;

    private String targetFramework;

    private Boolean includePreRelease;

    public String getFilter()
    {
        return filter;
    }

    public void setFilter(String filter)
    {
        this.filter = filter;
    }

    public String getSearchTerm()
    {
        return searchTerm;
    }

    public void setSearchTerm(String searchTerm)
    {
        this.searchTerm = searchTerm;
    }

    public String getTargetFramework()
    {
        return targetFramework;
    }

    public void setTargetFramework(String targetFramework)
    {
        this.targetFramework = targetFramework;
    }

    public Boolean getIncludePreRelease()
    {
        return includePreRelease;
    }

    public void setIncludePreRelease(Boolean includePreRelease)
    {
        this.includePreRelease = includePreRelease;
    }

}
