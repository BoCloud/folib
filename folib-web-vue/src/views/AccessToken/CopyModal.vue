<template>
  <a-modal v-model="visible" title="" :footer="null" :maskClosable="false" width="800px">

    <a-form-item class="mt-20" :label="$t('AccessToken.Token')">
      <div class="custom-input-group">
        <a-input
            v-model="tokenInfo.jwt"
            id="directory"
            ref="tokenInput"
        />
        <a-button
            type="link"
            class="copy-button btn"
            style="height: 40px"
            data-clipboard-action="copy"
            data-clipboard-target="#directory"
            @click="copyInput"
        >
          <a-icon type="copy"/>
        </a-button>
      </div>
    </a-form-item>

    <a-form-item>
      <a-alert
          :message="$t('AccessToken.NewTokenWarning')"
          type="warning"
          :show-icon="true"
      />
    </a-form-item>

    <div style="text-align: right;">
      <a-button type="primary" @click="closeModal">{{$t('AccessToken.Close')}}</a-button>
    </div>
  </a-modal>
</template>

<script>

import Clipboard from 'clipboard';
export default {
  name: "CopyModal",
  props: {
    tokenInfo: ""
  },
  data() {
    return {
      visible: false,
    }
  },
  methods: {
    openModal() {
      this.visible = true;
      if (this.visible) {
        this.$nextTick(() => {
              // 在模态框打开后聚焦到输入框
              setTimeout(() => {
                const inputElement = this.$refs.tokenInput.$refs.input;
                if (inputElement) {
                  inputElement.focus();
                  inputElement.select();
                }
              }, 100);
            }
        );
      }

    },
    copyInput() {
        const clipboard = new Clipboard('.btn')
        clipboard.on('success', () => {
            this.$message.success('令牌已复制到剪贴板')
            clipboard.destroy() // 使用destroy可以清除缓存
        })
    },
    closeModal() {
      this.visible = false;
    }
  }
}
</script>

<style scoped>
.custom-input-group {
  display: flex;
  width: 100%;
}

.custom-input-group {
  border-radius: 0;
}

.ant-input {
  border-radius: 0;
}

.custom-input-group .copy-button {
  border: 1px solid #d9d9d9;
  border-left: none;
  border-radius: 0;
  padding: 0 11px;
  font-size: 14px;
  height: 32px;
  line-height: 32px;
  background-color: white;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}

</style>