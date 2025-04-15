package com.veadan.folib.index.utils;

import com.veadan.folib.index.model.CondaVersion;
import lombok.Generated;
import org.apache.commons.lang3.math.NumberUtils;

import java.util.Comparator;
import java.util.Iterator;
import java.util.List;


/**
 * @author LingengMa
 * @date 2025/04/11 14:30
 * @Description:
 */

public class CondaVersionComparator implements Comparator<CondaVersion> {
    private static final CondaVersionComparator INSTANCE = new CondaVersionComparator();

    public static CondaVersionComparator get() {
        return INSTANCE;
    }

    public int compare(CondaVersion version1, CondaVersion version2) {
        if (version1 == null && version2 == null) {
            return 0;
        } else if (version1 == null) {
            return -1;
        } else if (version2 == null) {
            return 1;
        } else {
            int ans = this.comparePartList(version1.getCondaVersionParts(), version2.getCondaVersionParts());
            if (ans == 0) {
                ans = this.comparePartList(version1.getCondaLocalVersionParts(), version2.getCondaLocalVersionParts());
            }

            return ans;
        }
    }

    private int comparePartList(List<CondaVersion.CondaVersionPart> versionParts1, List<CondaVersion.CondaVersionPart> versionParts2) {
        CondaVersionPartComparator condaVersionPartComparator = CondaVersionPartComparator.get();
        Iterator<CondaVersion.CondaVersionPart> versionParts1Iterator = versionParts1.iterator();
        Iterator<CondaVersion.CondaVersionPart> versionParts2Iterator = versionParts2.iterator();

        int ans;
        for(ans = 0; ans == 0 && (versionParts1Iterator.hasNext() || versionParts2Iterator.hasNext()); ans = condaVersionPartComparator.compare(getVersionPart(versionParts1Iterator), getVersionPart(versionParts2Iterator))) {
        }

        return ans;
    }

    private static CondaVersion.CondaVersionPart getVersionPart(Iterator<CondaVersion.CondaVersionPart> versionParts1Iterator) {
        return versionParts1Iterator.hasNext() ? (CondaVersion.CondaVersionPart)versionParts1Iterator.next() : CondaVersion.CondaVersionPart.ZERO_PART;
    }

    @Generated
    private CondaVersionComparator() {
    }

    private static class CondaVersionPartComparator implements Comparator<CondaVersion.CondaVersionPart> {
        private static final CondaVersionPartComparator INSTANCE = new CondaVersionPartComparator();

        public static CondaVersionPartComparator get() {
            return INSTANCE;
        }

        public int compare(CondaVersion.CondaVersionPart versionPart1, CondaVersion.CondaVersionPart versionPart2) {
            Iterator<String> versionParts1Iterator = versionPart1.getCondaVersionSubParts().iterator();
            Iterator<String> versionParts2Iterator = versionPart2.getCondaVersionSubParts().iterator();

            int ans;
            for(ans = 0; ans == 0 && (versionParts1Iterator.hasNext() || versionParts2Iterator.hasNext()); ans = this.compare(versionParts1Iterator, versionParts2Iterator)) {
            }

            return ans;
        }

        private int compare(Iterator<String> o1Iterator, Iterator<String> o2Iterator) {
            String s1 = o1Iterator.hasNext() ? (String)o1Iterator.next() : "0";
            String s2 = o2Iterator.hasNext() ? (String)o2Iterator.next() : "0";
            int ans;
            if (NumberUtils.isDigits(s1)) {
                if (NumberUtils.isDigits(s2)) {
                    ans = Long.compare(Long.parseLong(s1), Long.parseLong(s2));
                } else {
                    ans = 1;
                }
            } else if (NumberUtils.isDigits(s2)) {
                ans = -1;
            } else {
                ans = s1.compareTo(s2);
            }

            return ans;
        }

        @Generated
        private CondaVersionPartComparator() {
        }
    }
}
