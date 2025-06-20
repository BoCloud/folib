package com.veadan.folib.model;

import com.google.common.base.Strings;
import javax.annotation.Nonnull;

public class HelmMaintainerModel {
    public String email;

    public String name;

    public String url;

    public HelmMaintainerModel() {}

    public HelmMaintainerModel(@Nonnull String name, String email, String url) {
        this.name = name;
        this.email = email;
        this.url = url;
    }

    public String toString() {
        return this.name + this.name + (Strings.isNullOrEmpty(this.email) ? "" : (" - " + this.email));
    }

    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof HelmMaintainerModel))
            return false;
        HelmMaintainerModel that = (HelmMaintainerModel)o;
        return (((this.name != null) ? this.name.equals(that.name) : (that.name == null)) && ((this.email != null) ? this.email
                .equals(that.email) : (that.email == null)) && ((this.url != null) ? this.url
                .equals(that.url) : (that.url == null)));
    }

    public int hashCode() {
        int result = (this.name != null) ? this.name.hashCode() : 0;
        result = 31 * result + ((this.email != null) ? this.email.hashCode() : 0);
        result = 31 * result + ((this.url != null) ? this.url.hashCode() : 0);
        return result;
    }
}