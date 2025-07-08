package com.folib.rpm;

public class RpmViewRequest {

    private String packageId;

    private String version;

    public String getPackageId()
    {
        return packageId;
    }

    public void setPackageId(String packageId)
    {
        this.packageId = packageId;
    }

    public String getVersion()
    {
        return version;
    }

    public void setVersion(String version)
    {
        this.version = version;
    }

}

