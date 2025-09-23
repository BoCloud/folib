package com.folib.index.model;

import com.google.common.collect.Lists;
import lombok.Generated;

import java.beans.ConstructorProperties;
import java.util.List;


public class CondaVersion {
    private List<CondaVersionPart> condaVersionParts;
    private List<CondaVersionPart> condaLocalVersionParts;

    @Generated
    public List<CondaVersionPart> getCondaVersionParts() {
        return this.condaVersionParts;
    }

    @Generated
    public List<CondaVersionPart> getCondaLocalVersionParts() {
        return this.condaLocalVersionParts;
    }

    @Generated
    public void setCondaVersionParts(final List<CondaVersionPart> condaVersionParts) {
        this.condaVersionParts = condaVersionParts;
    }

    @Generated
    public void setCondaLocalVersionParts(final List<CondaVersionPart> condaLocalVersionParts) {
        this.condaLocalVersionParts = condaLocalVersionParts;
    }

    @Generated
    public boolean equals(final Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof CondaVersion)) {
            return false;
        } else {
            CondaVersion other = (CondaVersion)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$condaVersionParts = this.getCondaVersionParts();
                Object other$condaVersionParts = other.getCondaVersionParts();
                if (this$condaVersionParts == null) {
                    if (other$condaVersionParts != null) {
                        return false;
                    }
                } else if (!this$condaVersionParts.equals(other$condaVersionParts)) {
                    return false;
                }

                Object this$condaLocalVersionParts = this.getCondaLocalVersionParts();
                Object other$condaLocalVersionParts = other.getCondaLocalVersionParts();
                if (this$condaLocalVersionParts == null) {
                    if (other$condaLocalVersionParts != null) {
                        return false;
                    }
                } else if (!this$condaLocalVersionParts.equals(other$condaLocalVersionParts)) {
                    return false;
                }

                return true;
            }
        }
    }

    @Generated
    protected boolean canEqual(final Object other) {
        return other instanceof CondaVersion;
    }

    @Generated
    public int hashCode() {
        int PRIME = 59;
        int result = 1;
        Object $condaVersionParts = this.getCondaVersionParts();
        result = result * 59 + ($condaVersionParts == null ? 43 : $condaVersionParts.hashCode());
        Object $condaLocalVersionParts = this.getCondaLocalVersionParts();
        result = result * 59 + ($condaLocalVersionParts == null ? 43 : $condaLocalVersionParts.hashCode());
        return result;
    }

    @Generated
    public String toString() {
        List var10000 = this.getCondaVersionParts();
        return "CondaVersion(condaVersionParts=" + var10000 + ", condaLocalVersionParts=" + this.getCondaLocalVersionParts() + ")";
    }

    @ConstructorProperties({"condaVersionParts", "condaLocalVersionParts"})
    @Generated
    public CondaVersion(final List<CondaVersionPart> condaVersionParts, final List<CondaVersionPart> condaLocalVersionParts) {
        this.condaVersionParts = condaVersionParts;
        this.condaLocalVersionParts = condaLocalVersionParts;
    }

    public static class CondaVersionPart {
        public static CondaVersionPart ZERO_PART = new CondaVersionPart(Lists.newArrayList(new String[]{"0"}));
        private List<String> condaVersionSubParts;

        @Generated
        public List<String> getCondaVersionSubParts() {
            return this.condaVersionSubParts;
        }

        @Generated
        public void setCondaVersionSubParts(final List<String> condaVersionSubParts) {
            this.condaVersionSubParts = condaVersionSubParts;
        }

        @Generated
        public boolean equals(final Object o) {
            if (o == this) {
                return true;
            } else if (!(o instanceof CondaVersionPart)) {
                return false;
            } else {
                CondaVersionPart other = (CondaVersionPart)o;
                if (!other.canEqual(this)) {
                    return false;
                } else {
                    Object this$condaVersionSubParts = this.getCondaVersionSubParts();
                    Object other$condaVersionSubParts = other.getCondaVersionSubParts();
                    if (this$condaVersionSubParts == null) {
                        if (other$condaVersionSubParts != null) {
                            return false;
                        }
                    } else if (!this$condaVersionSubParts.equals(other$condaVersionSubParts)) {
                        return false;
                    }

                    return true;
                }
            }
        }

        @Generated
        protected boolean canEqual(final Object other) {
            return other instanceof CondaVersionPart;
        }

        @Generated
        public int hashCode() {
            int PRIME = 59;
            int result = 1;
            Object $condaVersionSubParts = this.getCondaVersionSubParts();
            result = result * 59 + ($condaVersionSubParts == null ? 43 : $condaVersionSubParts.hashCode());
            return result;
        }

        @Generated
        public String toString() {
            return "CondaVersion.CondaVersionPart(condaVersionSubParts=" + this.getCondaVersionSubParts() + ")";
        }

        @ConstructorProperties({"condaVersionSubParts"})
        @Generated
        public CondaVersionPart(final List<String> condaVersionSubParts) {
            this.condaVersionSubParts = condaVersionSubParts;
        }
    }
}
