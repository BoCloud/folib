/*
 * Copyright 2019 Veadan Team & Development Ltd.
 * Copyright 2014 Dmitry Sviridov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.folib.storage.metadata.nuget;

import com.folib.artifact.coordinates.versioning.SemanticVersion;

import javax.annotation.Nonnull;
import java.io.Serializable;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Range of versions
 *
 * <p>
 * 1.0 = 1.0 ≤ x </p>
 * <p>
 * (, 1.0] = x ≤ 1.0 </p>
 * <p>
 * (, 1.0) = x <1.0 </p>
 * <p>
 * [1.0] = x == 1.0 </p>
 * <p>
 * (1.0) = invalid </p>
 * <p>
 * (1.0,) = 1.0 <x </p>
 * <p>
 * (1.0,2.0) = 1.0 <x <2.0 </p>
 * <p>
 * [1.0,2.0] = 1.0 ≤ x ≤ 2.0 </p>
 * <p>
 * empty = latest version </p>.
 *
 * @author sviridov
 */
public class VersionRange implements Serializable
{

    /**
     * Separator of the boundaries of the segment
     */
    public static final String BORDER_DELIMETER = ",";

    /**
     * Full version range template
     */
    public static final String FULL_VERSION_RANGE_PATTERN = "(?<leftBorder>[\\(\\[])"

            + "(?<left>([^" + BORDER_DELIMETER + "]+)?)"

            + BORDER_DELIMETER

            + "(?<right>([^" + BORDER_DELIMETER + "]+)?)"

            + "(?<rightBorder>[\\)\\]])";

    /**
     * Fixed version template
     */

    public static final String FIXED_VERSION_RANGE_PATTERN = "\\[([^\\]]+)\\]";

    /**
     * Version on the bottom border
     */
    private SemanticVersion lowVersion;

    /**
     * Bottom type
     */
    private BorderType lowBorderType;

    /**
     * Type upper bound
     */
    private BorderType topBorderType;

    /**
     * Version on the upper border
     */
    private SemanticVersion topVersion;

    /**
     * @return range indicates the latest version of the package.
     */
    public boolean isLatestVersion()
    {
        return lowVersion == null && topVersion == null;
    }

    /**
     * @return range indicates specific package version
     */
    public boolean isFixedVersion()
    {
        return lowVersion != null
                && topVersion != null
                && lowVersion.equals(topVersion);
    }

    /**
     * @return range indicates a specific or large version of the package.
     */
    public boolean isSimpleRange()
    {
        return topVersion == null
                && lowVersion != null
                && lowBorderType == BorderType.INCLUDE;
    }

    /**
     * Default constructor (all fields are null)
     */
    public VersionRange()
    {
    }

    /**
     * @param lowVersion
     *            Version on the bottom border
     * @param lowBorderType
     *            Type of lower bound
     * @param topVersion
     *            Version on the upper border
     * @param topBorderType
     *            Type of upper bound
     */
    public VersionRange(SemanticVersion lowVersion,
                        BorderType lowBorderType,
                        SemanticVersion topVersion,
                        BorderType topBorderType)
    {
        this.lowVersion = lowVersion;
        this.lowBorderType = lowBorderType;
        this.topBorderType = topBorderType;
        this.topVersion = topVersion;
    }

    /**
     * Type of range boundary
     */
    public enum BorderType
    {

        /**
         * Border included
         */
        INCLUDE("[",
                "]"),

        /**
         * Border excluded
         */
        EXCLUDE("(",
                ")");

        /**
         * Lower bound symbol
         */
        private final String lowBorderSymbol;

        /**
         * Upper bound symbol
         */
        private final String topBorderSymbol;

        /**
         * Recognizes border type
         *
         * @param borderSymbol
         * @return border type
         */
        private static BorderType getBorderType(String borderSymbol)
        {
            if (borderSymbol == null || borderSymbol.isEmpty())
            {
                return null;
            }
            else if (borderSymbol.equals(INCLUDE.lowBorderSymbol)
                    || borderSymbol.equals(INCLUDE.topBorderSymbol))
            {
                return INCLUDE;
            }
            else if (borderSymbol.equals(EXCLUDE.lowBorderSymbol)
                    || borderSymbol.equals(EXCLUDE.topBorderSymbol))
            {
                return EXCLUDE;
            }
            else
            {
                return null;
            }
        }

        /**
         * @param lowBorderSymbol
         *            symbol denoting the lower limit
         * @param topBorderSymbol
         *            symbol denoting the upper limit
         */
        private BorderType(String lowBorderSymbol,
                           String topBorderSymbol)
        {
            this.lowBorderSymbol = lowBorderSymbol;
            this.topBorderSymbol = topBorderSymbol;
        }

        /**
         * @return Lower bound character
         */
        public String getLowBorderSymbol()
        {
            return lowBorderSymbol;
        }

        /**
         * @return upper bound character
         */
        public String getTopBorderSymbol()
        {
            return topBorderSymbol;
        }
    }

    /**
     * @return Version on lower bound
     */
    public SemanticVersion getLowVersion()
    {
        return lowVersion;
    }

    /**
     * @param lowVersion
     *            Version on the bottom border
     */
    public void setLowVersion(SemanticVersion lowVersion)
    {
        this.lowVersion = lowVersion;
    }

    /**
     * @return Version on the upper border
     */
    public SemanticVersion getTopVersion()
    {
        return topVersion;
    }

    /**
     * @param topVersion
     *            Version on the upper border
     */
    public void setTopVersion(SemanticVersion topVersion)
    {
        this.topVersion = topVersion;
    }

    /**
     * @return type lower bound
     */
    public BorderType getLowBorderType()
    {
        return lowBorderType;
    }

    /**
     * @param lowBorderType
     *            Type of lower bound
     */
    public void setLowBorderType(BorderType lowBorderType)
    {
        this.lowBorderType = lowBorderType;
    }

    /**
     * @return type of upper bound
     */
    public BorderType getTopBorderType()
    {
        return topBorderType;
    }

    /**
     * @param topBorderType
     *            Type of upper bound
     */
    public void setTopBorderType(BorderType topBorderType)
    {
        this.topBorderType = topBorderType;
    }

    /**
     * @return string representation of a range of versions
     */
    @Override
    public String toString()
    {
        if (isLatestVersion())
        {
            return "";
        }

        if (isFixedVersion())
        {
            return "[" + topVersion.toString() + "]";
        }

        if (isSimpleRange())
        {
            return lowVersion.toString();
        }

        StringBuilder builder = new StringBuilder();
        if (lowVersion != null)
        {
            builder.append(lowBorderType.lowBorderSymbol);
            builder.append(lowVersion.toString());
        }
        else
        {
            builder.append(BorderType.EXCLUDE.lowBorderSymbol);
        }

        builder.append(BORDER_DELIMETER);

        if (topVersion != null)
        {
            builder.append(topVersion.toString());
            builder.append(topBorderType.topBorderSymbol);
        }
        else
        {
            builder.append(BorderType.EXCLUDE.topBorderSymbol);
        }

        return builder.toString();
    }

    /**
     * Recognizes the string and returns a range of versions.
     *
     * @param versionRangeString
     *            string of version ranges
     * @return version range
     * @throws NugetFormatException
     *             incorrect version format
     */
    public static VersionRange parse(String versionRangeString)
        throws NugetFormatException
    {
        if (versionRangeString == null || versionRangeString.isEmpty())
        {
            return new VersionRange();
        }

        SemanticVersion version = tryParseVersion(versionRangeString, true/* silent */);
        if (version != null)
        {
            return new VersionRange(version, BorderType.INCLUDE, null, null);
        }

        Pattern pattern = Pattern.compile("^" + FULL_VERSION_RANGE_PATTERN + "$");
        Matcher matcher = pattern.matcher(versionRangeString);
        if (matcher.matches())
        {
            SemanticVersion lowVersion = null;
            BorderType lowBorder = null;

            String lowVersionString = matcher.group("left");
            if (!lowVersionString.isEmpty())
            {
                lowVersion = tryParseVersion(lowVersionString, false/* silent */);
                lowBorder = BorderType.getBorderType(matcher.group("leftBorder"));
            }

            SemanticVersion topVersion = null;
            BorderType topBorder = null;

            String topVersionString = matcher.group("right");
            if (!topVersionString.isEmpty())
            {
                topVersion = tryParseVersion(topVersionString, false/* silent */);
                topBorder = BorderType.getBorderType(matcher.group("rightBorder"));
            }

            return new VersionRange(lowVersion, lowBorder, topVersion, topBorder);
        }

        Pattern fixedVersionPattern = Pattern.compile("^" + FIXED_VERSION_RANGE_PATTERN + "$");
        Matcher fixedVersionMatcher = fixedVersionPattern.matcher(versionRangeString);
        if (fixedVersionMatcher.matches())
        {
            version = tryParseVersion(fixedVersionMatcher.group(1), false/*
                                                                          * silent
                                                                          */);

            return new VersionRange(version, BorderType.INCLUDE, version, BorderType.INCLUDE);
        }

        throw new NugetFormatException(
                "<" + versionRangeString + "> does not match a semantic version or a version range.");
    }

    private static SemanticVersion tryParseVersion(@Nonnull
    final String versionString,
                                           boolean silent)
        throws NugetFormatException
    {
        try
        {
            SemanticVersion version = SemanticVersion.parse(versionString);

            return version;
        }
        catch (IllegalArgumentException e)
        {
            if (silent)
            {
                return null;
            }
            else
            {
                throw new NugetFormatException(e);
            }
        }
    }

    /**
     * @param obj
     *            object to compare with
     * @return true if version ranges are identical
     */
    @Override
    public boolean equals(Object obj)
    {
        if (obj == null)
        {
            return false;
        }

        if (getClass() != obj.getClass())
        {
            return false;
        }

        final VersionRange other = (VersionRange) obj;
        if (!Objects.equals(this.lowVersion, other.lowVersion))
        {
            return false;
        }

        if (this.lowBorderType != other.lowBorderType)
        {
            return false;
        }

        if (this.topBorderType != other.topBorderType)
        {
            return false;
        }

        return Objects.equals(this.topVersion, other.topVersion);
    }

    /**
     * @return HASH object code
     */
    @Override
    public int hashCode()
    {
        int hash = 7;
        hash = 59 * hash + Objects.hashCode(this.lowVersion);
        hash = 59 * hash + (this.lowBorderType != null ? this.lowBorderType.hashCode() : 0);
        hash = 59 * hash + (this.topBorderType != null ? this.topBorderType.hashCode() : 0);
        hash = 59 * hash + Objects.hashCode(this.topVersion);
        return hash;
    }
}
