export default {
    mounted(el, binding, vnode) {
      const componentInstance = vnode.component;
  
      if (componentInstance && componentInstance.type.name === 'ASelect') {
        // 检查是否已设置 getPopupContainer
        if (!componentInstance.props.getPopupContainer) {
          // 自动绑定 getPopupContainer
          componentInstance.props.getPopupContainer = (triggerNode) =>
            triggerNode.parentNode;
        }
      }
    },
  };
  