package com.veadan.folib.model;

/**
 * @author veadan
 * @date 2024/7/3
 **/

import com.fasterxml.jackson.annotation.JsonInclude;
import com.google.common.html.HtmlEscapers;
import lombok.*;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Nonnull;
import java.util.Objects;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PypiIndexEntry implements Comparable<PypiIndexEntry> {
    private static final String LINK_TEMPLATE = "<a href=\"%s\"%s rel=\"%s\">%s</a>";

    private String name;

    private String link;

    @Override
    @Generated
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof PypiIndexEntry)) {
            return false;
        }
        PypiIndexEntry other = (PypiIndexEntry) o;
        if (!other.canEqual(this)) {
            return false;
        }
        Object this$name = getName(), other$name = other.getName();
        return !((this$name == null) ? (other$name != null) : !this$name.equals(other$name));
    }

    @Generated
    protected boolean canEqual(Object other) {
        return other instanceof PypiIndexEntry;
    }

    @Generated
    public void setName(String name) {
        this.name = name;
    }

    @Generated
    public void setLink(String link) {
        this.link = link;
    }

    @Generated
    public String getName() {
        return this.name;
    }

    @Generated
    public String getLink() {
        return this.link;
    }

    @Override
    public String toString() {
        return String.format("<a href=\"%s\">%s</a>", this.link, this.name);
    }

    private void appendNonEmptyPropertyToStringBuilder(StringBuilder stringBuilder, String propertyName, String propertyValue) {
        if (StringUtils.isNotEmpty(propertyValue)) {
            appendPropertyToStringBuilder(stringBuilder, propertyName, propertyValue);
        }
    }

    private void appendNonNullPropertyToStringBuilder(StringBuilder stringBuilder, String propertyName, String propertyValue) {
        if (propertyValue != null) {
            appendPropertyToStringBuilder(stringBuilder, propertyName, propertyValue);
        }
    }

    private void appendPropertyToStringBuilder(StringBuilder stringBuilder, String propertyName, String propertyValue) {
        String propertyEscaped = HtmlEscapers.htmlEscaper().escape(propertyValue);
        stringBuilder.append(" ").append(propertyName).append("=\"").append(propertyEscaped).append("\"");
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public int compareTo(@Nonnull PypiIndexEntry o) {
        if (this.name == null) {
            return -1;
        }
        if (o.name == null) {
            return 1;
        }
        return this.name.compareTo(o.name);
    }
}

