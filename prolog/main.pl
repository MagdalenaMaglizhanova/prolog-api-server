import { useState, useRef, useEffect } from "react";
import { useParams } from "react-router-dom";
import { collection, getDocs } from "firebase/firestore";
import { db } from "../services/firebase";
import styles from "./PrologChat.module.css";

interface Message {
  user: boolean;
  text: string;
  id: string;
  timestamp: Date;
}

interface PrologCode {
  id: string;
  code: string;
  title?: string;
}

interface Domain {
  id: string;
  label: string;
  icon: string;
  description: string;
}

export default function PrologChat() {
  const { codeId } = useParams<{ codeId?: string }>();
  const [messages, setMessages] = useState<Message[]>([]);
  const [query, setQuery] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [allCodes, setAllCodes] = useState<PrologCode[]>([]);
  const [activeCode, setActiveCode] = useState<PrologCode | null>(null);
  const [isLoadingCode, setIsLoadingCode] = useState(true);
  const [activeTab, setActiveTab] = useState<"chat" | "code" | "domains">("chat");
  const [selectedDomain, setSelectedDomain] = useState<string | null>(null);
  const [isLoadingDomain, setIsLoadingDomain] = useState(false);
  const messagesEndRef = useRef<HTMLDivElement>(null);

  // Domains configuration
  const domains: Domain[] = [
    { id: "animals", label: "Animals", icon: "fas fa-paw", description: "Animal facts and relationships" },
    { id: "history", label: "History", icon: "fas fa-landmark", description: "Historical events and figures" },
    { id: "geography", label: "Geography", icon: "fas fa-globe-americas", description: "Geographical facts and locations" },
    { id: "general", label: "General KB", icon: "fas fa-database", description: "General knowledge base" }
  ];

  const quickQueries = [
    { label: "Help", query: "help.", icon: "fas fa-question-circle" },
    { label: "List Files", query: "list_files.", icon: "fas fa-list" },
    { label: "Clear Chat", query: "clear.", icon: "fas fa-trash-alt" },
    { label: "Load All", query: "load_all.", icon: "fas fa-folder-open" },
    { label: "Active File", query: "current_file.", icon: "fas fa-file-alt" },
    { label: "System Info", query: "system_info.", icon: "fas fa-info-circle" },
  ];

  const codeExamples = [
    { label: "Animal Facts", code: "animal(dog).\nanimal(cat).\npet(X) :- animal(X)." },
    { label: "Family Tree", code: "parent(john, mary).\nparent(mary, ann).\ngrandparent(X, Z) :- parent(X, Y), parent(Y, Z)." },
    { label: "Math Rules", code: "add(0, X, X).\nadd(s(X), Y, s(Z)) :- add(X, Y, Z)." },
  ];

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(() => {
    requestAnimationFrame(() => {
      scrollToBottom();
    });
  }, [messages]);

  // Load all Prolog codes from database
  useEffect(() => {
    async function loadAllCodes() {
      setIsLoadingCode(true);
      try {
        const snapshot = await getDocs(collection(db, "prologCodes"));
        const codes: PrologCode[] = snapshot.docs.map(docSnap => ({
          id: docSnap.id,
          code: (docSnap.data() as { code: string }).code,
          title: (docSnap.data() as { title?: string }).title,
        }));
        setAllCodes(codes);

        // If codeId provided, set it as active
        if (codeId) {
          const specific = codes.find(c => c.id === codeId);
          if (specific) {
            setActiveCode(specific);
          }
        }
      } catch (err) {
        console.error("Failed to load Prolog codes:", err);
      } finally {
        setIsLoadingCode(false);
      }
    }
    loadAllCodes();
  }, [codeId]);

  // Load domain
  const loadDomain = async (domain: string) => {
    setIsLoadingDomain(true);
    setSelectedDomain(domain);
    setActiveCode(null); // Clear active code when switching domains

    const thinkingMsg: Message = {
      user: false,
      text: `Loading ${domain} domain...`,
      id: "thinking-" + Date.now().toString(),
      timestamp: new Date()
    };
    setMessages(prev => [...prev, thinkingMsg]);

    try {
      // Try to load domain from the new API endpoint
      const res = await fetch("https://prolog.onrender.com/prolog/select-domain", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ domain: domain })
      });

      if (res.ok) {
        const data = await res.json();
        setMessages(prev => [
          ...prev.filter(msg => msg.id !== thinkingMsg.id),
          {
            user: false,
            text: data.message || `✅ Domain '${domain}' loaded successfully.\nYou can now query the knowledge base.`,
            id: Date.now().toString(),
            timestamp: new Date()
          }
        ]);
      } else {
        // Fallback to using a code if API endpoint not available
        throw new Error("Domain API not available");
      }
    } catch (err: any) {
      // Fallback: Use local domain knowledge if available
      const domainCode = allCodes.find(c => c.id.includes(domain) || c.title?.toLowerCase().includes(domain));
      if (domainCode) {
        setActiveCode(domainCode);
        setMessages(prev => [
          ...prev.filter(msg => msg.id !== thinkingMsg.id),
          {
            user: false,
            text: `✅ Loaded domain '${domain}' from local knowledge base.\n${domainCode.title || domainCode.id}`,
            id: Date.now().toString(),
            timestamp: new Date()
          }
        ]);
      } else {
        setMessages(prev => [
          ...prev.filter(msg => msg.id !== thinkingMsg.id),
          {
            user: false,
            text: `⚠️ Domain '${domain}' not found in local knowledge base. You can still query using general Prolog commands.`,
            id: Date.now().toString(),
            timestamp: new Date()
          }
        ]);
      }
    } finally {
      setIsLoadingDomain(false);
    }
  };

  const sendQuery = async (customQuery?: string) => {
    const finalQuery = customQuery ?? query;
    if (!finalQuery.trim() || isLoading) return;

    // Handle clear command
    if (finalQuery.trim() === "clear.") {
      setMessages([]);
      setQuery("");
      setSelectedDomain(null);
      setActiveCode(null);
      return;
    }

    // Handle examples command
    if (finalQuery.trim() === "examples.") {
      const examplesMessage = codeExamples.map(ex => `${ex.label}:\n${ex.code}`).join("\n\n");
      setMessages(prev => [...prev, {
        user: false,
        text: `Available Examples:\n\n${examplesMessage}`,
        id: Date.now().toString(),
        timestamp: new Date(),
      }]);
      setQuery("");
      return;
    }

    const userMsg: Message = {
      user: true,
      text: finalQuery,
      id: Date.now().toString(),
      timestamp: new Date(),
    };
    setMessages(prev => [...prev, userMsg]);
    setQuery("");

    // Check if query matches a code activation command
    const matchingCode = allCodes.find(c =>
      c.code.includes(`${finalQuery} :-`) || c.code.includes(`${finalQuery}.`)
    );
    if (matchingCode) {
      setActiveCode(matchingCode);
      setSelectedDomain(null);
      setMessages(prev => [...prev, {
        user: false,
        text: `✅ Activated code: ${matchingCode.title || matchingCode.id}`,
        id: Date.now().toString(),
        timestamp: new Date(),
      }]);
    }

    // Determine which endpoint to use based on active context
    let endpoint = "https://prolog.onrender.com/prolog-run";
    let body: any = { query: finalQuery };

    if (selectedDomain) {
      // Use domain API endpoint
      endpoint = "https://prolog.onrender.com/prolog/command";
      body = { command: finalQuery };
    } else if (activeCode) {
      // Use code-based endpoint
      body.code = activeCode.code;
    } else if (!activeCode && !selectedDomain) {
      // No active context - use system commands only
      setMessages(prev => [...prev, {
        user: false,
        text: "⚠️ No active domain or code. Please select a domain from the Domains tab or activate a code file.\n\nYou can still use system commands:\n• help.\n• list_files.\n• load_all.\n• consult_file('filename').",
        id: Date.now().toString(),
        timestamp: new Date(),
      }]);
      return;
    }

    setIsLoading(true);

    // Add thinking message
    const thinkingMsg: Message = {
      user: false,
      text: "Thinking",
      id: "thinking-" + Date.now().toString(),
      timestamp: new Date(),
    };
    setMessages(prev => [...prev, thinkingMsg]);

    // Animate the dots
    let dotCount = 1;
    const dotInterval = setInterval(() => {
      dotCount = (dotCount % 3) + 1;
      const dots = ".".repeat(dotCount);
      setMessages(prev =>
        prev.map(msg =>
          msg.id === thinkingMsg.id ? { ...msg, text: `Thinking${dots}` } : msg
        )
      );
    }, 500);

    try {
      const res = await fetch(endpoint, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(body),
      });

      const text = await res.text();
      
      // Clear interval and remove thinking message
      clearInterval(dotInterval);
      setMessages(prev => prev.filter(msg => msg.id !== thinkingMsg.id));
      
      const botMsg: Message = {
        user: false,
        text: text || "No response from server.",
        id: Date.now().toString(),
        timestamp: new Date(),
      };
      setMessages(prev => [...prev, botMsg]);
    } catch (err: any) {
      // Clear interval and remove thinking message, then add error
      clearInterval(dotInterval);
      setMessages(prev => prev.filter(msg => msg.id !== thinkingMsg.id));
      setMessages(prev => [...prev, {
        user: false,
        text: `❌ Error: ${err.message || "Failed to connect to server"}`,
        id: Date.now().toString(),
        timestamp: new Date(),
      }]);
    }
    setIsLoading(false);
  };

  const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      sendQuery();
    }
  };

  const formatCode = (code: string) =>
    code
      .split("\n")
      .map(line => {
        if (line.trim().startsWith("%")) return `<span class="${styles.codeComment}">${line}</span>`;
        if (line.includes(":-")) return `<span class="${styles.codeRule}">${line}</span>`;
        if (line.trim().endsWith(".")) return `<span class="${styles.codeFact}">${line}</span>`;
        if (line.includes("?-")) return `<span class="${styles.codeQuery}">${line}</span>`;
        return line;
      })
      .join("\n");

  const handleCodeSelect = (code: PrologCode) => {
    setActiveCode(code);
    setSelectedDomain(null);
    setMessages(prev => [...prev, {
      user: false,
      text: `✅ Activated code: ${code.title || code.id}`,
      id: Date.now().toString(),
      timestamp: new Date(),
    }]);
  };

  const clearAll = () => {
    setMessages([]);
    setSelectedDomain(null);
    setActiveCode(null);
    setQuery("");
  };

  return (
    <div className={styles.prologChat}>
      {/* SIDEBAR */}
      <aside className={styles.sidebar}>
        <div className={styles.sidebarContent}>
          <div className={styles.userInfo}>
            <div className={styles.userAvatar}>
              <img src="/images/logo_shevici.jpg" alt="Digital Bulgaria" />
            </div>
            <div className={styles.userEmail}>Prolog AI Assistant</div>
            <h3 className={styles.welcomeText}>Interactive Knowledge Base</h3>
          </div>

          <div className={styles.sidebarStats}>
            <div className={styles.statsTitle}>
              <i className="fas fa-chart-bar"></i>
              System Status
            </div>
            <div className={styles.statsGrid}>
              <div className={styles.statCard}>
                <div className={styles.statValue}>{messages.filter(m => m.user).length}</div>
                <div className={styles.statLabel}>Queries</div>
              </div>
              <div className={styles.statCard}>
                <div className={styles.statValue}>{allCodes.length}</div>
                <div className={styles.statLabel}>Code Files</div>
              </div>
              <div className={styles.statCard}>
                <div className={styles.statValue}>
                  {selectedDomain ? (
                    <i className="fas fa-check-circle" style={{ color: "#10b981" }}></i>
                  ) : activeCode ? (
                    <i className="fas fa-file-code" style={{ color: "#3b82f6" }}></i>
                  ) : (
                    <i className="fas fa-times-circle" style={{ color: "#ef4444" }}></i>
                  )}
                </div>
                <div className={styles.statLabel}>
                  {selectedDomain ? "Domain Active" : activeCode ? "Code Active" : "No Context"}
                </div>
              </div>
            </div>
          </div>

          <div className={styles.codeSelector}>
            <div className={styles.codeSelectorHeader}>
              <h4><i className="fas fa-file-code"></i> Local Knowledge Bases</h4>
            </div>
            <div className={styles.codeList}>
              {isLoadingCode ? (
                <div className={styles.loadingState}>
                  <i className="fas fa-spinner fa-spin"></i>
                  <span>Loading codes...</span>
                </div>
              ) : allCodes.length === 0 ? (
                <div className={styles.emptyState}>
                  <i className="fas fa-file-alt"></i>
                  <span>No codes available</span>
                </div>
              ) : (
                allCodes.map((code) => (
                  <button
                    key={code.id}
                    className={`${styles.codeItem} ${activeCode?.id === code.id ? styles.codeItemActive : ''}`}
                    onClick={() => handleCodeSelect(code)}
                  >
                    <div className={styles.codeIcon}>
                      <i className="fas fa-file-code"></i>
                    </div>
                    <div className={styles.codeInfo}>
                      <div className={styles.codeTitle}>
                        {code.title || `Code ${code.id.slice(0, 8)}`}
                      </div>
                      <div className={styles.codePreview}>
                        {code.code.substring(0, 40)}...
                      </div>
                    </div>
                  </button>
                ))
              )}
            </div>
          </div>

          <div className={styles.systemActions}>
            <button 
              className={styles.systemButton}
              onClick={clearAll}
              disabled={isLoading}
            >
              <i className="fas fa-trash-alt"></i>
              Clear All
            </button>
            <button 
              className={styles.systemButton}
              onClick={() => sendQuery("help.")}
              disabled={isLoading}
            >
              <i className="fas fa-question-circle"></i>
              System Help
            </button>
          </div>
        </div>
      </aside>

      {/* MAIN CONTENT */}
      <main className={styles.mainContent}>
        <div className={styles.header}>
          <div className={styles.titleContainer}>
            <h1 className={styles.title}>
              Prolog AI Console
              <span className={styles.titleHighlight}>
                {selectedDomain ? ` | ${selectedDomain} Domain` : 
                 activeCode ? ` | ${activeCode.title || 'Active Code'}` : 
                 ' | Select Context'}
              </span>
            </h1>
            <div className={styles.contextIndicator}>
              {selectedDomain && (
                <span className={styles.contextBadge}>
                  <i className="fas fa-folder"></i>
                  Domain: {selectedDomain}
                </span>
              )}
              {activeCode && (
                <span className={styles.contextBadge}>
                  <i className="fas fa-file-code"></i>
                  Code: {activeCode.title || activeCode.id.slice(0, 8)}
                </span>
              )}
              <div className={styles.dateIndicator}>
                <i className="far fa-clock"></i>
                {new Date().toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}
              </div>
            </div>
          </div>

          <div className={styles.tabNavigation}>
            <button
              className={`${styles.tabButton} ${activeTab === "chat" ? styles.tabButtonActive : ''}`}
              onClick={() => setActiveTab("chat")}
            >
              <i className="fas fa-comments"></i>
              Chat
            </button>
            <button
              className={`${styles.tabButton} ${activeTab === "domains" ? styles.tabButtonActive : ''}`}
              onClick={() => setActiveTab("domains")}
            >
              <i className="fas fa-folder"></i>
              Domains
            </button>
            <button
              className={`${styles.tabButton} ${activeTab === "code" ? styles.tabButtonActive : ''}`}
              onClick={() => setActiveTab("code")}
            >
              <i className="fas fa-code"></i>
              Code Preview
            </button>
          </div>
        </div>

        <div className={styles.content}>
          {/* CHAT TAB */}
          {activeTab === "chat" && (
            <div className={styles.chatSection}>
              <div className={styles.quickQueries}>
                {quickQueries.map((btn, idx) => (
                  <button
                    key={idx}
                    onClick={() => sendQuery(btn.query)}
                    disabled={isLoading || isLoadingDomain}
                    className={styles.quickQueryButton}
                  >
                    <i className={btn.icon}></i>
                    {btn.label}
                  </button>
                ))}
              </div>

              <div className={styles.chatWindow}>
                <div className={styles.messagesContainer}>
                  {(messages.length === 0 ? [{ 
                    user: false, 
                    text: "🤖 Welcome to Prolog AI Console!\n\nI can help you query knowledge bases in different domains. To get started:\n1. Go to the 'Domains' tab and select a knowledge domain\n2. Or activate a code file from the sidebar\n3. Then ask questions about the knowledge base!\n\nTry: 'help.' for system commands or 'load_all.' to load all files", 
                    id: "welcome", 
                    timestamp: new Date() 
                  }] : messages).map(msg => (
                    <div key={msg.id} className={`${styles.messageWrapper} ${msg.user ? styles.userMessage : styles.botMessage}`}>
                      <div className={styles.messageContent}>
                        {!msg.user && (
                          <div className={styles.messageAvatar}>
                            <div className={styles.userAvatar}>
                              <img src="/images/logo_shevici.jpg" alt="Digital Bulgaria" />
                            </div>
                          </div>
                        )}
                        <div className={`${styles.messageBubble} ${msg.id.startsWith('thinking-') ? styles.thinkingBubble : ''}`}>
                          <div className={`${styles.messageText} ${msg.id.startsWith('thinking-') ? styles.thinkingText : ''}`}>
                            {msg.text.split("\n").map((line, i) => (
                              <div key={i} className={styles.messageLine}>
                                {line}
                              </div>
                            ))}
                          </div>
                          <div className={styles.messageTime}>
                            {msg.timestamp.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })}
                          </div>
                        </div>
                        {msg.user && (
                          <div className={styles.messageAvatar}>
                            <div className={styles.userAvatar}>
                              <i className="fas fa-user"></i>
                            </div>
                          </div>
                        )}
                      </div>
                    </div>
                  ))}
                  <div ref={messagesEndRef} className={styles.scrollAnchor} />
                </div>

                <div className={styles.inputContainer}>
                  <form onSubmit={(e) => e.preventDefault()} className={styles.inputForm}>
                    <div className={styles.inputWrapper}>
                      <input
                        type="text"
                        value={query}
                        onChange={e => setQuery(e.target.value)}
                        onKeyDown={handleKeyDown}
                        placeholder={
                          selectedDomain ? `Ask about ${selectedDomain} domain...` :
                          activeCode ? `Query ${activeCode.title || 'active code'}...` :
                          "Select a domain or code first..."
                        }
                        className={styles.chatInput}
                        disabled={isLoading || isLoadingDomain || (!selectedDomain && !activeCode)}
                      />
                      <button
                        type="button"
                        onClick={() => sendQuery()}
                        disabled={isLoading || isLoadingDomain || !query.trim() || (!selectedDomain && !activeCode)}
                        className={styles.sendButton}
                      >
                        {isLoading ? (
                          <i className="fas fa-spinner fa-spin"></i>
                        ) : (
                          <>
                            <i className="fas fa-paper-plane"></i>
                            <span className={styles.sendButtonText}>Send</span>
                          </>
                        )}
                      </button>
                    </div>
                    <div className={styles.inputHint}>
                      <i className="fas fa-info-circle"></i>
                      Press Enter to send • Shift+Enter for new line • Use Prolog syntax
                    </div>
                  </form>
                </div>
              </div>
            </div>
          )}

          {/* DOMAINS TAB */}
          {activeTab === "domains" && (
            <div className={styles.domainsSection}>
              <div className={styles.domainsHeader}>
                <h2 className={styles.domainsTitle}>
                  <i className="fas fa-folder-tree"></i>
                  Knowledge Domains
                </h2>
                <p className={styles.domainsSubtitle}>
                  Select a knowledge domain to query. Each domain contains specialized facts and rules.
                </p>
              </div>

              <div className={styles.domainsGrid}>
                {domains.map(domain => (
                  <button
                    key={domain.id}
                    className={`${styles.domainCard} ${selectedDomain === domain.id ? styles.domainActive : ''}`}
                    onClick={() => loadDomain(domain.id)}
                    disabled={isLoadingDomain}
                  >
                    <div className={styles.domainIcon}>
                      <i className={domain.icon}></i>
                    </div>
                    <div className={styles.domainContent}>
                      <h3 className={styles.domainName}>{domain.label}</h3>
                      <p className={styles.domainDescription}>{domain.description}</p>
                      <div className={styles.domainStatus}>
                        {selectedDomain === domain.id ? (
                          <span className={styles.domainActiveBadge}>
                            <i className="fas fa-check-circle"></i>
                            Active
                          </span>
                        ) : (
                          <span className={styles.domainInactiveBadge}>
                            Click to activate
                          </span>
                        )}
                      </div>
                    </div>
                  </button>
                ))}
              </div>

              {selectedDomain && (
                <div className={styles.domainCommands}>
                  <h3 className={styles.domainCommandsTitle}>
                    <i className="fas fa-terminal"></i>
                    Example Queries for {selectedDomain}
                  </h3>
                  <div className={styles.domainCommandsGrid}>
                    <div className={styles.commandExample}>
                      <div className={styles.commandCode}>list_files.</div>
                      <div className={styles.commandDesc}>List loaded domain files</div>
                    </div>
                    <div className={styles.commandExample}>
                      <div className={styles.commandCode}>listing.</div>
                      <div className={styles.commandDesc}>Show all facts and rules</div>
                    </div>
                    <div className={styles.commandExample}>
                      <div className={styles.commandCode}>{selectedDomain === 'animals' ? 'animal(X).' : selectedDomain === 'history' ? 'event(Event, Year).' : 'location(Place).'}</div>
                      <div className={styles.commandDesc}>Query domain-specific facts</div>
                    </div>
                    <div className={styles.commandExample}>
                      <div className={styles.commandCode}>help.</div>
                      <div className={styles.commandDesc}>Show available commands</div>
                    </div>
                  </div>
                </div>
              )}
            </div>
          )}

          {/* CODE TAB */}
          {activeTab === "code" && (
            <div className={styles.codeSection}>
              <div className={styles.codeHeader}>
                <h2 className={styles.codeTitle}>
                  <i className="fas fa-file-code"></i>
                  {selectedDomain ? `Domain: ${selectedDomain}` :
                   activeCode ? (activeCode.title || `Code: ${activeCode.id}`) : 
                   "No Active Context"}
                </h2>
                {activeCode && (
                  <div className={styles.codeActions}>
                    <button 
                      className={styles.codeActionButton}
                      onClick={() => {
                        navigator.clipboard.writeText(activeCode.code);
                        // Add feedback message
                        setMessages(prev => [...prev, {
                          user: false,
                          text: "✅ Code copied to clipboard!",
                          id: Date.now().toString(),
                          timestamp: new Date(),
                        }]);
                      }}
                    >
                      <i className="fas fa-copy"></i>
                      Copy Code
                    </button>
                    <button className={styles.codeActionButton}>
                      <i className="fas fa-download"></i>
                      Export
                    </button>
                  </div>
                )}
              </div>

              <div className={styles.codeEditorContainer}>
                {selectedDomain ? (
                  <div className={styles.domainInfo}>
                    <div className={styles.domainInfoIcon}>
                      <i className="fas fa-info-circle"></i>
                    </div>
                    <div className={styles.domainInfoContent}>
                      <h3>Domain: {selectedDomain}</h3>
                      <p>The {selectedDomain} domain is currently active. You can query it using Prolog syntax in the Chat tab.</p>
                      <p>Domain knowledge is loaded from the server and contains specialized facts and rules about {domains.find(d => d.id === selectedDomain)?.description.toLowerCase()}.</p>
                      <button 
                        className={styles.switchToChatButton}
                        onClick={() => setActiveTab("chat")}
                      >
                        <i className="fas fa-comments"></i>
                        Switch to Chat to Query
                      </button>
                    </div>
                  </div>
                ) : activeCode ? (
                  <div className={styles.codeEditor}>
                    <div className={styles.codeToolbar}>
                      <span className={styles.codeMeta}>
                        <i className="fas fa-file-alt"></i>
                        {activeCode.title || "Untitled Code"} • {activeCode.code.split('\n').length} lines
                      </span>
                    </div>
                    <pre className={styles.prologCode}>
                      <code dangerouslySetInnerHTML={{ __html: formatCode(activeCode.code) }} />
                    </pre>
                  </div>
                ) : (
                  <div className={styles.noCode}>
                    <i className="fas fa-file-code"></i>
                    <h3>No active context</h3>
                    <p>Select a domain from the Domains tab or activate a code file from the sidebar to preview it here.</p>
                    <button 
                      className={styles.browseButton}
                      onClick={() => setActiveTab("domains")}
                    >
                      <i className="fas fa-folder"></i>
                      Browse Domains
                    </button>
                  </div>
                )}
              </div>
            </div>
          )}
        </div>
      </main>
    </div>
  );
}
