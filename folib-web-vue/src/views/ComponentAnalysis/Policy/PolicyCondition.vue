<template>
  <div class="bar-content1">
    <a-row type="flex" justify="space-between">
      <a-col :span="8">
        <!-- 条件1 -->
        <a-form-item label="">
          <a-select style="width: 320px" @change="subjectChanged" v-model="subject">
            <a-select-option :value="item.value" v-for="item in subjects" :key="item.value"> {{ item.text }} </a-select-option>
          </a-select>
        </a-form-item>
      </a-col>
      <a-col>
        <!-- 条件2 -->
        <a-form-item label="">
          <a-select style="width: 320px" v-model="operator">
            <a-select-option :value="item.value" v-for="item in operators" :key="item.value"> {{ item.text }} </a-select-option>
          </a-select>
        </a-form-item>
      </a-col>
      <!-- 条件3 -->
      <a-col>
        <a-form-item label="" v-if="subject !== 'COORDINATES' && isSubjectSelectable">
          <a-select style="width: 320px" v-model="value" show-search :filter-option="filterOption" @change="saveCondition">
            <a-select-option :value="item.value" v-for="item in possibleValues" :key="item.value"> {{ item.text }} </a-select-option>
          </a-select>
        </a-form-item>
        <a-form-item label="" v-else-if="subject !== 'COORDINATES' && !isSubjectSelectable">
          <a-input v-model="value" placeholder="名称" class="add-name" @change="saveCondition" />
        </a-form-item>
        <a-form-item label="" v-else-if="subject === 'COORDINATES'" style="display: flex">
          <a-input v-model="coordinatesGroup" placeholder="组" style="width: 180px" @change="saveCondition" />
          <a-input v-model="coordinatesName" placeholder="名称" style="width: 180px" @change="saveCondition" />
          <a-input v-model="coordinatesVersion" placeholder="版本" style="width: 180px" @change="saveCondition" />
        </a-form-item>
      </a-col>
      <a-col>
        <a-icon type="delete" theme="twoTone" two-tone-color="#f86c6b" @click="handleDeleteCondition()" style="font-size: 20px" />
      </a-col>
    </a-row>
  </div>
</template>

<script>
import { getPolicyLicenceList, getPolicyLicenceGroupList, editPolicyList, addPolicyList, delPolicyList } from "@/api/policy.js";
import { trimToNull } from "@/utils/util";

export default {
  props: {
    policy: Object,
    condition: Object,
  },
  created() {
    // console.log(this.condition);
    if (this.condition) {
      this.subject = this.condition.subject;
      this.subjectChanged();
      this.operator = this.condition.operator;
      this.value = this.condition.value;
    }
  },
  data() {
    return {
      subject: null,
      operator: null,
      value: null,
      coordinatesGroup: null,
      coordinatesName: null,
      coordinatesVersion: null,
      subjects: [
        { value: "AGE", text: "年龄" },
        { value: "SEVERITY", text: "严重性" },
        { value: "COORDINATES", text: "坐标" },
        { value: "LICENSE", text: "许可证" },
        { value: "LICENSE_GROUP", text: "许可证组" },
        { value: "PACKAGE_URL", text: "PURL" },
        { value: "CPE", text: "通用平台枚举" },
        { value: "SWID_TAGID", text: "标签标识" },
        { value: "VERSION", text: "版本" },
        { value: "COMPONENT_HASH", text: "组件hash" },
        { value: "CWE", text: "常见漏洞类型枚举" },
        { value: "VULNERABILITY_ID", text: "漏洞编号" },
      ],
      numericOperators: [
        { value: "NUMERIC_GREATER_THAN", text: ">" },
        { value: "NUMERIC_LESS_THAN", text: "<" },
        { value: "NUMERIC_EQUAL", text: "=" },
        { value: "NUMERIC_NOT_EQUAL", text: "≠" },
        { value: "NUMERIC_GREATER_THAN_OR_EQUAL", text: "≥" },
        { value: "NUMERIC_LESSER_THAN_OR_EQUAL", text: "≤" },
      ],
      objectOperators: [
        { value: "IS", text: "是" },
        { value: "IS_NOT", text: "否" },
      ],
      regexOperators: [
        { value: "MATCHES", text: "匹配" },
        { value: "NO_MATCH", text: "不匹配" },
      ],
      hashAlgorithms: [
        { value: "MD5", text: "MD5" },
        { value: "SHA-1", text: "SHA-1" },
        { value: "SHA-256", text: "SHA-256" },
        { value: "SHA-384", text: "SHA-384" },
        { value: "SHA-512", text: "SHA-512" },
        { value: "SHA3-256", text: "SHA3-256" },
        { value: "SHA3-384", text: "SHA3-384" },
        { value: "SHA3-512", text: "SHA3-512" },
        { value: "BLAKE2b-256", text: "BLAKE2b-256" },
        { value: "BLAKE2b-384", text: "BLAKE2b-384" },
        { value: "BLAKE2b-512", text: "BLAKE2b-512" },
        { value: "BLAKE3", text: "BLAKE3" },
      ],
      listOperators: [
        { value: "CONTAINS_ANY", text: "包含所有" },
        { value: "CONTAINS_ALL", text: "包含任何" },
      ],
      operators: [],
      possibleValues: [],
    };
  },
  computed: {
    isSubjectSelectable: function () {
      switch (this.subject) {
        case "AGE":
          return false;
        case "ANALYZER":
          return true;
        case "BOM":
          return true;
        case "SEVERITY":
          return true;
        case "COORDINATES":
          return false;
        case "LICENSE":
          return true;
        case "LICENSE_GROUP":
          return true;
        case "PACKAGE_URL":
          return false;
        case "CPE":
          return false;
        case "SWID_TAGID":
          return false;
        case "VERSION":
          return false;
        case "COMPONENT_HASH":
          return false;
        case "CWE":
          return false;
        case "VULNERABILITY_ID":
          return false;
        default:
          return false;
      }
    },
  },
  beforeMount() {
    if (this.subject === "COORDINATES") {
      let v = JSON.parse(this.value);
      // console.log(v);

      if (v) {
        this.coordinatesGroup = v.group;
        this.coordinatesName = v.name;
        this.coordinatesVersion = v.version;
      }
    }
  },
  methods: {
    subjectChanged: function () {
      switch (this.subject) {
        case "AGE":
          this.operators = this.numericOperators;
          break;
        case "ANALYZER":
          this.operators = this.objectOperators;
          break;
        case "BOM":
          this.operators = this.objectOperators;
          break;
        case "SEVERITY":
          this.operators = this.objectOperators;
          this.populateSeverity();
          break;
        case "COORDINATES":
          this.operators = this.regexOperators;
          break;
        case "LICENSE":
          this.operators = this.objectOperators;
          this.retrieveLicenses();
          break;
        case "LICENSE_GROUP":
          this.operators = this.objectOperators;
          this.retrieveLicenseGroups();
          break;
        case "PACKAGE_URL":
          this.operators = this.regexOperators;
          break;
        case "CPE":
          this.operators = this.regexOperators;
          break;
        case "SWID_TAGID":
          this.operators = this.regexOperators;
          break;
        case "VERSION":
          this.operators = this.numericOperators;
          break;
        case "COMPONENT_HASH":
          this.operators = this.hashAlgorithms;
          break;
        case "CWE":
          this.operators = this.listOperators;
          break;
        case "VULNERABILITY_ID":
          this.operators = this.objectOperators;
          break;
        default:
          this.operators = [];
      }
      this.saveCondition();
    },
    createDynamicValue() {
      if (this.subject === "COORDINATES") {
        return JSON.stringify({
          group: trimToNull(this.coordinatesGroup),
          name: trimToNull(this.coordinatesName),
          version: trimToNull(this.coordinatesVersion),
        });
      } else if (this.subject === "COMPONENT_HASH") {
        return JSON.stringify({
          algorithm: trimToNull(this.operator),
          value: trimToNull(this.value),
        });
      } else {
        return this.value;
      }
    },
    saveCondition() {
      let dynamicValue = this.createDynamicValue();
      if (!this.subject || !this.operator || !dynamicValue) {
        return;
      }
      if (this.condition.uuid) {
        const params = {
          uuid: this.condition.uuid,
          subject: this.subject,
          operator: this.subject === "COMPONENT_HASH" ? "IS" : this.operator,
          value: dynamicValue,
        };
        editPolicyList(params)
          .then((response) => {
            this.condition = response.data;
          })
          .catch((error) => {});
      } else {
        const params = {
          subject: this.subject,
          operator: this.subject === "COMPONENT_HASH" ? "IS" : this.operator,
          value: dynamicValue,
        };
        addPolicyList(this.policy.uuid, params)
          .then((response) => {
            this.condition = response.data;
          })
          .catch((error) => {});
      }
    },
    // removeCondition: function () {
    //   if (this.condition && this.condition.uuid) {
    //     let url = `${this.$api.BASE_URL}/${this.$api.URL_POLICY}/condition/${this.condition.uuid}`;
    //     this.axios
    //       .delete(url)
    //       .then((response) => {
    //         this.condition = response.data;
    //         this.$toastr.s(this.$t("message.condition_deleted"));
    //         this.$emit("conditionRemoved");
    //       })
    //       .catch((error) => {
    //         this.$toastr.w(this.$t("condition.unsuccessful_action"));
    //       });
    //   } else {
    //     this.$emit("conditionRemoved");
    //   }
    // },
    // 获取证书组 getPolicyLicenceGroupList
    retrieveLicenseGroups() {
      getPolicyLicenceGroupList()
        .then((response) => {
          let vals = [];
          for (let i = 0; i < response.data.length; i++) {
            let object = response.data[i];
            vals.push({ value: object.uuid, text: object.name });
          }
          this.possibleValues = vals;
        })
        .catch((error) => {});
    },
    // 获取证书
    retrieveLicenses() {
      const params = {
        pageSize: 9999,
        pageNumber: 1,
        searchText: this.searchText,
      };
      getPolicyLicenceList(params)
        .then((response) => {
          let vals = [];
          vals.push({ value: "unresolved", text: "unresolved" });
          for (let i = 0; i < response.data.length; i++) {
            let object = response.data[i];
            vals.push({ value: object.uuid, text: object.name });
          }
          this.possibleValues = vals;
        })
        .catch((error) => {});
    },
    populateSeverity() {
      this.possibleValues = [
        { value: "CRITICAL", text: "危急" },
        { value: "HIGH", text: "高" },
        { value: "MEDIUM", text: "中危" },
        { value: "LOW", text: "低" },
        { value: "INFO", text: "信息" },
        { value: "UNASSIGNED", text: "未末分配" },
      ];
    },
    valueInputTooltip: function () {
      switch (this.subject) {
        case "AGE":
          return this.$t("message.age_tooltip");
        default:
          return "";
      }
    },
    filterOption(input, option) {
      return option.componentOptions.children[0].text.toLowerCase().indexOf(input.toLowerCase()) >= 0;
    },
    // 删除政策
    handleDeleteCondition() {
      if (this.condition && this.condition.uuid) {
        delPolicyList(this.condition.uuid)
          .then((response) => {
            // 获取数据接口
          })
          .catch((error) => {});
      } else {
        // 获取数据接口
      }
    },
  },
};
</script>

<style lang="scss" scoped>
.bar-content1 {
  width: 100%;
}
.add-name {
  width: 320px;
}
</style>
