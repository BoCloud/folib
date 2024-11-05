<template>
  <div>
    <div style="margin: 0">
      <div class="by-flex by-row-between by-col-top">
        <div class="disabled-textarea by-m-r-20">
          <div class="textarea-label">{{ $t('Projects.Condition') }}</div>
          <a-input
              :defaultValue="getConditionVal(record.policyCondition)"
              type="textarea"
              disabled
              :rows="6"
          ></a-input>
        </div>
        <div class="disabled-textarea">
          <div class="textarea-label">{{ $t('Projects.AuditTrail') }}</div>
          <a-input
              :defaultValue="getAuditVal(record)"
              v-model="commentsData"
              type="textarea"
              disabled
              :rows="6"
          ></a-input>
        </div>
      </div>
      <div class="commit by-m-t-20">
        <div class="commit-label">{{ $t('Projects.Commit') }}</div>
        <a-input
            v-model="commit"
            :placeholder="$t('Projects.CommitPlaceholder')"
            type="textarea"
            :rows="6"
            class="by-m-t-10 by-m-b-10"
        ></a-input>
        <a-button class="add-commit" @click="addCommit(record)">{{ $t('Projects.AddCommit') }}</a-button>
      </div>
      <div class="by-flex by-m-t-20">
        <div class="by-flex">
          <span class="by-m-r-10">{{ $t('Projects.Analysis') }}</span>
          <a-select
              v-model="analysisState"
              class="analysis"
              @change="handleAnalysisChange"
          >
            <a-select-option
                v-for="(item, i) in analysisOptions"
                :value="item.value"
                :key="i"
            >
              {{ $t(`Projects.${item.label}`) }}
            </a-select-option>
          </a-select>
        </div>
        <div class="by-m-l-45 by-flex">
          <span class="by-m-r-10">{{ $t('Projects.Suppressed') }}</span>
          <a-switch
              v-model="isSuppressed"
              checked-children="是"
              un-checked-children="否"
              @change="onSuppressChange"
          />
        </div>
      </div>
    </div>
  </div>
</template>
<script>
import {formatTimestamp} from "@/utils/util";
import {addViolationAnalysis} from "@/api/projects";

export default {
  name: "PolicyExpandedContent",
  computed: {
    i18nColumns() {
      return this.columns.map((column) => {
        if (column.i18nKey) {
          column.title = this.$t(column.i18nKey)
        }
        return column
      })
    },
    lang() {
      return this.$store.state.language.lang
    },
  },
  props: {
    record: {
      type: Object,
      default() {
        return {}
      }
    }
  },
  data() {
    return {
      visible: false,
      loading: false,
      commit: '',
      analysisState: 'NOT_SET',
      analysisOptions: [
        {
          label: 'NotSet',
          value: 'NOT_SET'
        },
        {
          label: 'Approved',
          value: 'APPROVED'
        },
        {
          label: 'Rejected',
          value: 'REJECTED'
        },
      ],
      isSuppressed: false,
      analysisRowData: {},
      violationRowData: {},
      commentsData: ''

    }
  },
  created() {
    this.open();
  },
  methods: {
    formatTimestamp,
    open() {
      this.violationRowData = this.record;
      this.analysisRowData = this.record.analysis ? this.record.analysis : {};
    },
    clone(){
      this.violationRowData = {};
      this.analysisRowData = {};
      this.commentsData = '';
      this.analysisState='NOT_SET';
    },
    getConditionVal({subject, operator, value}) {
      return `subject == ${subject} && value ${operator} ${value}`
    },
    getAuditVal(comments) {
      // console.log("comments0", comments);
      // this.violationRowData = comments;
      // this.analysisRowData = comments.analysis ? comments.analysis : {};
      // console.log("analysisRowData", this.analysisRowData);
      // console.log("analysisComments", this.analysisRowData.analysisComments);
      if (!this.analysisRowData || !this.analysisRowData.analysisComments) {
        return;
      }
      //console.log("analysisRowData", this.analysisRowData);
      let comment = ''
      this.analysisRowData.analysisComments.forEach(item => {
        // 保持返回样式，勿动
        comment +=
            `${item.commenter} - ${this.formatTimestamp(item.timestamp, true, this.lang)}
${item.comment}
`
      })
      this.commentsData = comment;
      return comment
    },

    onSuppressChange(checked) {
      this.isSuppressed = checked;
      let data = {
        "policyViolation": this.violationRowData.uuid,
        "component": this.violationRowData.component.uuid,
        "analysisState": this.analysisState,
        "suppressed": checked
      }
      addViolationAnalysis(data).then((res) => {
        if (res.status === 200) {
          this.$notification.success({
            message: this.$t('Projects.UpdateMessage'),
            description: "",
          });
          this.getData(res);
        }
      }).catch(error => {
        this.$message.error(error.response.data)
      }).finally(() => {
      });
    },

    addCommit(params) {
      let data = {
        "policyViolation": params.uuid,
        "component": params.component.uuid,
        "analysisState": this.analysisState,
        "comment": this.commit,
        "suppressed": this.isSuppressed
      }
      addViolationAnalysis(data).then((res) => {
        if (res.status === 200) {
          this.$notification.success({
            message: this.$t('Projects.UpdateMessage'),
            description: "",
          });
          let result = res.data;
          //console.log("res", result);
          //this.violationRowData = result;
          //console.log(" this.violationRowData", this.violationRowData);
          this.analysisRowData = result ? result : {};
          this.commit = '';
          //console.log("this.analysisRowData",  this.analysisRowData );
          this.getAuditVal()
        }
      }).catch(error => {
        this.$message.error(error.response.data)
      }).finally(() => {
      });
    },
    handleAnalysisChange(value) {
      this.analysisState = value;
      let data = {
        "policyViolation": this.violationRowData.uuid,
        "component": this.violationRowData.component.uuid,
        "analysisState": this.analysisState,
        "suppressed": this.isSuppressed
      }
      addViolationAnalysis(data).then((res) => {
        if (res.status === 200) {
          this.$notification.success({
            message: this.$t('Projects.UpdateMessage'),
            description: "",
          });
          this.getData(res);
        }
      }).catch(error => {
        this.$message.error(error.response.data)
      }).finally(() => {
      });
    },
    getData(res) {
      let result = res.data;
      this.analysisRowData = result ? result : {};
      this.getAuditVal()
    }
  }
}

</script>
<style lang="scss" scoped>
.search {
  height: 50px;
}

.mx-25 .ant-row-flex {
  flex-wrap: wrap;
}

.v-search {
  max-width: 200px;
  width: 170px;
  min-width: 150px;
  margin-left: 5px;
  margin-bottom: 8px;
}

::v-deep .is-expand {
  transform: rotate(90deg);
  transition: all 0.5s;
}
::v-deep .disabled-textarea {
  background: #ffffff;
  width: 50%;
  border: 1px solid #E8E8E8;
  border-radius: 4px;

  .ant-input[disabled]:hover {
    border-color: transparent;
  }

  .ant-input[disabled] {
    border-color: transparent;
    background: transparent;
    color: #092943;
  }
}

.textarea-label {
  display: inline-block;
  height: 24px;
  line-height: 24px;
  padding: 0 20px;
  font-weight: 600;
  font-size: 14px;
  color: #FFFFFF;
  background: #007AFF;
  border-bottom-right-radius: 16px;
}

.commit {
  padding: 10px 20px;
  background: #FFFFFF;
  border: 1px solid #D8DDE4;

  .commit-label {
    display: inline-block;
    font-weight: 600;
    font-size: 16px;
    color: #324569;
    padding: 3px 10px;
    border-bottom: solid 2px #1890FF;
  }

  .add-commit {
    background: rgba(0, 144, 255, 0.1);
    color: #0090FF;
    box-shadow: none;
    border: none;
  }
}

::v-deep .analysis .ant-select-selection{
  background: #0090FF;
  color: #FFFFFF;
  height: 32px;

  .ant-select-selection-selected-value {
    padding-right: 10px;
    height: 32px;
    line-height: 30px;
  }

  .ant-select-arrow-icon {
    color: #FFFFFF;
  }
}
</style>