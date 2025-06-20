package com.veadan.folib.model.publish;

import com.veadan.folib.utils.CollectionUtils;

import java.util.List;

public class CargoPublishRes {

    private CargoPublishWarnings warnings;


    public CargoPublishRes() {
    }


    public CargoPublishWarnings getWarnings() {
        return this.warnings;
    }

    public CargoPublishRes(List<String> invalidCategories, List<String> invalidBadges, List<String> other) {
        this.warnings = new CargoPublishWarnings(invalidCategories, invalidBadges, other);
    }

    public boolean warningsAbsent() {
        return (CollectionUtils.isNullOrEmpty(this.warnings.invalidCategories) &&
                CollectionUtils.isNullOrEmpty(this.warnings.invalidBadges) &&
                CollectionUtils.isNullOrEmpty(this.warnings.other));
    }

    public static class CargoPublishWarnings {
        private List<String> invalidCategories;

        private List<String> invalidBadges;

        private List<String> other;


        private CargoPublishWarnings() {
        }

        public CargoPublishWarnings(List<String> invalidCategories, List<String> invalidBadges, List<String> other) {
            this.invalidCategories = invalidCategories;
            this.invalidBadges = invalidBadges;
            this.other = other;
        }


        public List<String> getInvalidCategories() {
            return this.invalidCategories;
        }

        public List<String> getInvalidBadges() {
            return this.invalidBadges;
        }

        public List<String> getOther() {
            return this.other;
        }
    }
}
