<template>
  <div>


    <div style="margin: 0">
      <div class="by-flex by-row-between by-col-top">
        <div class="disabled-textarea by-m-r-20">
          <div class="textarea-label">{{ $t('Projects.Description') }}</div>
          <a-input
              :defaultValue="record.vulnerability.description"
              type="textarea"
              disabled
              :rows="6"
          ></a-input>
        </div>
        <div class="disabled-textarea">
          <div class="textarea-label">{{ $t('Projects.AuditTrail') }}</div>
          <a-input
              type="textarea"
              v-model="commentsData"
              :defaultValue="commentsData"
              disabled
              :rows="6"
          ></a-input>
        </div>
      </div>
      <div class="commit by-m-t-20">
        <div class="commit-label">{{ $t('Projects.Commit') }}</div>
        <a-input
            :placeholder="$t('Projects.CommitPlaceholder')"
            type="textarea"
            :rows="6"
            :defaultValue="comment"
            v-model="comment"
            class="by-m-t-10 by-m-b-10"

        ></a-input>
        <a-button class="add-commit" @click="addCommit">{{
            $t('Projects.AddCommit')
          }}
        </a-button>
      </div>
      <div class="by-flex by-m-t-20">
        <div class="by-flex">
          <a-tooltip placement="topLeft" :title="$t('Projects.AnalysisPrompt')"
                     arrow-point-at-center>
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
          </a-tooltip>
        </div>
        <div class="by-m-l-45 by-flex">
          <span class="by-m-r-10">{{ $t('Projects.Suppressed') }}</span>
          <a-switch
              v-model="isSuppressed"
              checked-children="是"
              un-checked-children="否"
              @change="handleSuppressedChange"
          />
        </div>
        <div class="by-m-l-45 by-flex">
          <a-tooltip placement="topLeft" :title="$t('Projects.JustificationPrompt')"
                     arrow-point-at-center>
            <span class="by-m-r-10">{{ $t('Projects.Justification') }}</span>
            <a-select
                v-model="justificationState"
                class="analysis"
                :disabled="isJustification"
                @change="handleJustificationChange"
            >
              <a-select-option
                  v-for="(item, i) in justificationOptions"
                  :value="item.value"
                  :key="i"
              >
                {{ $t(`Projects.${item.label}`) }}
              </a-select-option>
            </a-select>
          </a-tooltip>
        </div>
        <div class="by-m-l-45 by-flex">
          <a-tooltip placement="topLeft" :title="$t('Projects.VendorResponsePrompt')"
                     arrow-point-at-center>
            <span class="by-m-r-10">{{ $t('Projects.VendorResponse') }}</span>
            <a-select
                v-model="vendorResponseState"
                class="analysis"
                @change="handleVendorResponseChange"
            >
              <a-select-option
                  v-for="(item, i) in vendorResponseOptions"
                  :value="item.value"
                  :key="i"
              >
                {{ $t(`Projects.${item.label}`) }}
              </a-select-option>
            </a-select>
          </a-tooltip>
        </div>

      </div>
      <div class="commit by-m-t-20">
        <div class="commit-label">{{ $t('Projects.Details') }}</div>
        <a-tooltip placement="topLeft" :title="$t('Projects.DetailsPrompt')"
                   arrow-point-at-center>
          <a-input
              :placeholder="$t('Projects.DetailsPlaceholder')"
              type="textarea"
              :rows="6"
              class="by-m-t-10 by-m-b-10"
              v-model="analysisDetails"
          ></a-input>
          <a-button class="add-commit" @click="addCommit">{{
              $t('Projects.UpdateDetails')
            }}
          </a-button>
        </a-tooltip>
      </div>
    </div>
  </div>
</template>
<script>
import {addVulnerabilitiesAnalysis, getVulnerabilitiesAnalysis} from "@/api/projects";
import {formatTimestamp} from "@/utils/util";

export default {
  name: "VulnExpandedContent",
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
      fileList: [],
      uuid: '',
      analysisState: 'NOT_SET',
      justificationState: 'NOT_SET',
      vendorResponseState: 'NOT_SET',
      analysisDetails: '',
      isJustification: true,
      isSuppressed: false,
      projectUuid: null,
      componentUuid: null,
      vulnerabilityUuid: null,
      analysisComments: [],
      commentsData: '',
      comment: '',
      analysisOptions: [
        {
          label: 'NotSet',
          value: 'NOT_SET'
        },
        {
          label: 'Exploitable',
          value: 'EXPLOITABLE'
        },
        {
          label: 'InTriage',
          value: 'IN_TRIAGE'
        },
        {
          label: 'Resolved',
          value: 'REJECTED'
        },
        {
          label: 'FalsePositive',
          value: 'FALSE_POSITIVE'
        },
        {
          label: 'NotAffected',
          value: 'NOT_AFFECTED'
        }
      ],
      vendorResponseOptions: [
        {
          label: 'NotSet',
          value: 'NOT_SET'
        },
        {
          label: 'CanNotFix',
          value: 'CAN_NOT_FIX'
        },
        {
          label: 'WillNotFix',
          value: 'WILL_NOT_FIX'
        },
        {
          label: 'Update',
          value: 'update'
        },
        {
          label: 'Rollback',
          value: 'ROLLBACK'
        },
        {
          label: 'WorkaroundAvailable',
          value: 'WORKAROUND_AVAILABLE'
        },
      ],
      justificationOptions: [
        {
          label: 'NotSet',
          value: 'NOT_SET'
        },
        {
          label: 'CodeNotPresent',
          value: 'CODE_NOT_PRESENT'
        },
        {
          label: 'CodeNotReachable',
          value: 'CODE_NOT_REACHABLE'
        },
        {
          label: 'RequiresConfiguration',
          value: 'REQUIRES_CONFIGURATION'
        },
        {
          label: 'RequiresDependency',
          value: 'REQUIRES_DEPENDENCY'
        },
        {
          label: 'RequiresEnvironment',
          value: 'REQUIRES_ENVIRONMENT'
        },
        {
          label: 'ProtectedByCompiler',
          value: 'PROTECTED_BY_COMPILER'
        },
        {
          label: 'ProtectedAtRuntime',
          value: 'PROTECTED_AT_RUNTIME'
        },
        {
          label: 'ProtectedAtPerimeter',
          value: 'PROTECTED_AT_PERIMETER'
        }
      ],
    }
  },
  methods: {
    formatTimestamp,
    handleSuppressedChange() {
      this.addCommit();
    },
    handleJustificationChange(){
      this.addCommit();
    },
    handleVendorResponseChange(){
      this.addCommit();
    },
    handleAnalysisChange() {
      if (this.analysisState === 'NOT_AFFECTED') {
        this.isJustification = false
      } else {
        this.isJustification = true
      }
      this.addCommit();
    },
    open() {
      this.visible = true;
      this.isSuppressed = this.record.analysis.isSuppressed;
      this.analysisState = this.record.analysis.state ? this.record.analysis.state : this.analysisState;
      this.projectUuid = this.record.component.project;
      this.componentUuid = this.record.component.uuid;
      this.vulnerabilityUuid = this.record.vulnerability.uuid;
      getVulnerabilitiesAnalysis(this.projectUuid, this.componentUuid, this.vulnerabilityUuid).then((res) => {
        if (res.status === 200) {
          this.isSuppressed = res.data.suppressed;
          this.analysisState = res.data.state ? res.data.state : this.analysisState;
          this.justificationState = res.data.analysisJustification ? res.data.analysisJustification : this.justificationState;
          this.vendorResponseState = res.data.analysisResponse ? res.data.analysisResponse : this.vendorResponseState;
          this.analysisDetails = res.data.analysisDetails ? res.data.analysisDetails : this.analysisDetails;
          this.analysisComments = res.data.analysisComments;
          this.getAuditVal();
        }
      }).finally(() => {
        this.loading = false
      });
    },
    close(){
      this.visible= false;
      this.loading = false;
      this.analysisState = 'NOT_SET';
      this.justificationState = 'NOT_SET';
      this.fileList = [];
      this.uuid = '';
      this.vendorResponseState = 'NOT_SET';
      this.analysisDetails = '';
      this.isJustification = true;
      this.isSuppressed = false;
      this.projectUuid = null;
      this.componentUuid = null;
      this.vulnerabilityUuid = null;
      this.analysisComments = [];
      this.commentsData = '';
      this.comment = '';
      this.visible = true;
    },
    addCommit() {
      const data = {
        "project": this.projectUuid,
        "component": this.componentUuid,
        "vulnerability": this.vulnerabilityUuid,
        "analysisState": this.analysisState,
        "analysisJustification": this.justificationState,
        "analysisResponse": this.vendorResponseState,
        "analysisDetails": this.analysisDetails === '' ? null : this.analysisDetails,
        "comment": this.comment === '' ? null : this.comment,
        "isSuppressed": this.isSuppressed
      }
      addVulnerabilitiesAnalysis(data).then((res) => {
        if (res.status === 200) {
          this.isSuppressed = res.data.isSuppressed ? res.data.isSuppressed : false;
          this.analysisState = res.data.state ? res.data.state : this.analysisState;
          this.justificationState = res.data.analysisJustification ? res.data.analysisJustification : this.justificationState;
          this.vendorResponseState = res.data.analysisResponse ? res.data.analysisResponse : this.vendorResponseState;
          this.analysisDetails = res.data.analysisDetails ? res.data.analysisDetails : this.analysisDetails;
          this.analysisComments = res.data.analysisComments;
          this.$notification.success({
            message: this.$t('Projects.UpdateMessage'),
            description: "",
          });
          this.getAuditVal();
        }
      }).finally(() => {
        this.loading = false
      });
    },
    getAuditVal() {
      if (!this.analysisComments || this.analysisComments.length === 0) {
        return;
      }
      //console.log("analysisRowData", this.analysisRowData);
      let comment = ''
      this.analysisComments.forEach(item => {
        // 保持返回样式，勿动
        comment +=
            `${item.commenter} - ${this.formatTimestamp(item.timestamp, true, this.lang)}
${item.comment}
`
      })
      this.commentsData = comment;
      return comment
    },
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

::v-deep .analysis .ant-select-selection {
  background: #0090FF;
  color: #FFFFFF;
  height: 32px;
  width: 150px;

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