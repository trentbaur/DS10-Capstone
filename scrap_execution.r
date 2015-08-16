

debug(run_model)
debug(evaluate_model)
debug(process_document)
debug(process_documents)
debug(process_documents_old)
debug(model_gram)
debug(model_backoff_simple)
debug(model_backoff_stupid)

undebug(run_model)
undebug(evaluate_model)
undebug(process_document)
undebug(process_documents)
undebug(process_documents_old)
undebug(model_gram)
undebug(model_backoff_simple)
undebug(model_backoff_stupid)



predict_word('This is a test to try to')


evaluate_model(textdata[1:20], model_backoff_simple, ' ', 2)


evaluate_model(textdata[1:20], model_backoff_stupid)


evaluate_model(textdata[1:20], model_gram, ' ', 3)

run_model(textdata[2:4], model_gram, ' ', 2)

run_model(textdata[2:4], model_gram, ' ', 3)

run_model(textdata[2:14], model_gram, ' ', 4)

results <- run_model(textdata[1:20], model_gram, ' ', 2)


run_model(textdata[2], model_gram, ' ', 4)

q<-process_documents_old(textdata[2], model_gram, ' ', 3)


q <- process_documents(textdata[2], model_gram, ' ', 3)
class(q)

q[[1]][[1]]




textdata[2:14][, document := stri_split_fixed(document, ' '), with=FALSE]



evaluate_model(data.table((c('This is a test to try to'))), model_gram, ' ', 3)


tail(run_model(data.table((c('This is a test to try to'))), model_gram, ' ', 3), n=3)
tail(run_model(data.table((c('This is a test to try to'))), model_gram, ' ', 3), n=3)$guess
